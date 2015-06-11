.biocExtraPackages <- c(
  "nlcv",
  "org.TguttataTestingSubset.eg.db",
  "RCurl",
  "Rlibstree",
  "SNPRelate",
  "SSOAP",
  "SVGAnnotation",
  "XMLRPC",
  "XMLSchema"
)


bundleFiles <- function(appDir) {
  # determine the files that will be in the bundle (exclude rsconnect dir
  # as well as common hidden files)
  files <- list.files(appDir, recursive = TRUE, all.files = TRUE,
                      full.names = FALSE)
  files <- files[!grepl(glob2rx("rsconnect/*"), files)]
  files <- files[!grepl(glob2rx(".svn/*"), files)]
  files <- files[!grepl(glob2rx(".git/*"), files)]
  files <- files[!grepl(glob2rx(".Rproj.user/*"), files)]
  files <- files[!grepl(glob2rx("*.Rproj"), files)]
  files <- files[!grepl(glob2rx(".DS_Store"), files)]
  files <- files[!grepl(glob2rx(".gitignore"), files)]
  files <- files[!grepl(glob2rx("packrat/*"), files)]
  files
}

bundleApp <- function(appName, appDir, appFiles, appPrimaryDoc, contentCategory,
                      accountInfo) {

  # create a directory to stage the application bundle in
  bundleDir <- tempfile()
  dir.create(bundleDir, recursive = TRUE)
  on.exit(unlink(bundleDir), add = TRUE)

  # infer the mode of the application from its layout
  appMode <- inferAppMode(appDir, appFiles)

  # copy the files into the bundle dir
  for (file in appFiles) {
    from <- file.path(appDir, file)
    to <- file.path(bundleDir, file)
    if (!file.exists(dirname(to)))
      dir.create(dirname(to), recursive = TRUE)
    file.copy(from, to)
  }

  if (!isShinyapps(accountInfo)) {
    # infer package dependencies for non-static content deployment
    if (appMode != "static") {
      addPackratSnapshot(bundleDir, appMode)
    }
  }

  # get application users
  users <- suppressWarnings(authorizedUsers(if (is.null(appPrimaryDoc))
                                                appDir
                                            else
                                                file.path(appDir, appPrimaryDoc)))

  # generate the manifest and write it into the bundle dir
  manifestJson <- enc2utf8(createAppManifest(bundleDir, appMode,
                                             contentCategory, accountInfo,
                                             appFiles, appPrimaryDoc, users))
  writeLines(manifestJson, file.path(bundleDir, "manifest.json"),
             useBytes = TRUE)

  # if necessary write an index.htm for shinydoc deployments
  indexFiles <- writeRmdIndex(appName, bundleDir)
  on.exit(unlink(indexFiles), add = TRUE)

  # create the bundle and return its path
  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir), add = TRUE)
  bundlePath <- tempfile("rsconnect-bundle", fileext = ".tar.gz")
  utils::tar(bundlePath, files = ".", compression = "gzip")
  bundlePath
}

isShinyRmd <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  delim <- grep("^---\\s*$", lines)
  if (length(delim) >= 2) {
    # If at least two --- lines were found...
    if (delim[[1]] == 1 || all(grepl("^\\s*$", lines[1:delim[[1]]]))) {
      # ...and the first --- line is not preceded by non-whitespace...
      if (diff(delim[1:2]) > 1) {
        # ...and there is actually something between the two --- lines...
        yamlData <- paste(lines[(delim[[1]] + 1):(delim[[2]] - 1)],
                          collapse = "\n")
        frontMatter <- yaml::yaml.load(yamlData)
        runtime <- frontMatter[["runtime"]]
        if (!is.null(runtime) && identical(runtime, "shiny")) {
          # ...and "runtime: shiny", then it's a dynamic Rmd.
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

inferAppMode <- function(appDir, files) {
  shinyFiles <- grep("^(server|app).r$", files, ignore.case = TRUE, perl = TRUE)
  if (length(shinyFiles) > 0) {
    return("shiny")
  }

  rmdFiles <- grep("^[^/\\\\]+\\.rmd$", files, ignore.case = TRUE, perl = TRUE,
                   value = TRUE)

  # if there are one or more R Markdown documents, use the Shiny app mode if any
  # are Shiny documents
  if (length(rmdFiles) > 0) {
    for (rmdFile in rmdFiles) {
      if (isShinyRmd(file.path(appDir, rmdFile)))
        return("rmd-shiny")
    }
    return("rmd-static")
  }

  # no renderable content here; if there's at least one file, we can just serve
  # it as static content
  if (length(files) > 0) {
    return("static")
  }

  # there doesn't appear to be any content here we can use
  return(NA)
}

createAppManifest <- function(appDir, appMode, contentCategory, accountInfo,
                              files, appPrimaryDoc, users) {

  # provide package entries for all dependencies
  packages <- list()
  # potential error messages
  msg      <- NULL

  # for apps which run code, discover dependencies
  if (appMode != "static") {
    for (pkg in dirDependencies(appDir)) {

      # get the description
      description <- list(description = suppressWarnings(
        utils::packageDescription(pkg)))

      # if description is NA, application dependency may not be installed
      if (is.na(description)) {
        msg <- c(msg, paste("Application depends on package \"", pkg, "\" but ",
                            "it is not installed. Please resolve before ",
                            "continuing.", sep = ""))
        next
      }

      # get package repository (e.g. source)
      repo <-  getRepository(description[[1]])

      # validate the repository (returns an error message if there is a problem)
      msg <- c(msg, validateRepository(pkg, repo))

      # append the bioc version to any bioconductor packages
      # TODO: resolve against actual BioC repo a package was pulled from
      # (in case the user mixed and matched)
      if (identical(repo, "BioC")) {

        # capture Bioc repository if available
        biocPackages = available.packages(contriburl=contrib.url(BiocInstaller::biocinstallRepos(),
                                                                 type="source"))
        if (pkg %in% biocPackages) {
          description$description$biocRepo <- biocPackages[pkg, 'Repository']
        }

        description$description$biocVersion <- BiocInstaller::biocVersion()
      }

      # good to go
      packages[[pkg]] <- description
    }
  }
  if (length(msg)) stop(paste(formatUL(msg, '\n*'), collapse = '\n'), call. = FALSE)

  # provide checksums for all files
  filelist <- list()
  for (file in files) {
    checksum <- list(checksum = digest::digest(file.path(appDir, file),
                                               algo = "md5", file = TRUE))
    filelist[[file]] <- I(checksum)
  }

  # if deploying an R Markdown app or static content, infer a primary document
  # if not already specified
  if ((grepl("rmd", appMode, fixed = TRUE) || appMode == "static")
      && is.null(appPrimaryDoc)) {
    # determine expected primary document extension
    ext <- ifelse(appMode == "static", "html?", "Rmd")

    # use index file if it exists
    primary <- which(grepl(paste0("^index\\.", ext, "$"), files, fixed = FALSE,
                           ignore.case = TRUE))
    if (length(primary) == 0) {
      # no index file found, so pick the first one we find
      primary <- which(grepl(paste0("^.*\\.", ext, "$"), files, fixed = FALSE,
                             ignore.case = TRUE))
      if (length(primary) == 0) {
        stop("Application mode ", appMode, " requires at least one document.")
      }
    }
    # if we have multiple matches, pick the first
    if (length(primary) > 1)
      primary <- primary[[1]]
    appPrimaryDoc <- files[[primary]]
  }

  # create userlist
  userlist <- list()
  if (!is.null(users) && length(users) > 0) {
    for (i in 1:nrow(users)) {
      user <- users[i, "user"]
      hash <- users[i, "hash"]
      userinfo <- list()
      userinfo$hash <- hash
      userlist[[user]] <- userinfo
    }
  }

  # create the manifest
  manifest <- list()
  manifest$version <- 1
  #manifest$locale <- getOption('rsconnect.locale', detectLocale())
  manifest$platform <- paste(R.Version()$major, R.Version()$minor, sep = ".")

  metadata <- list(appmode = appMode)

  # emit appropriate primary document information
  primaryDoc <- ifelse(is.null(appPrimaryDoc), NA, appPrimaryDoc)
  metadata$primary_rmd <- ifelse(grepl("\\brmd\\b", appMode), primaryDoc, NA)
  metadata$primary_html <- ifelse(appMode == "static", primaryDoc, NA)

  # emit content category (plots, etc)
  metadata$content_category <- ifelse(!is.null(contentCategory),
                                      contentCategory, NA)
  # add metadata
  manifest$metadata <- metadata

  # if there are no packages set manifes$packages to NA (json null)
  if (length(packages) > 0) {
    manifest$packages <- I(packages)
  } else {
    manifest$packages <- NA
  }
  # if there are no files, set manifest$files to NA (json null)
  if (length(files) > 0) {
    manifest$files <- I(filelist)
  } else {
    manifest$files <- NA
  }
  # if there are no users set manifest$users to NA (json null)
  if (length(users) > 0) {
    manifest$users <- I(userlist)
  } else {
    manifest$users <- NA
  }

  # return it as json
  RJSONIO::toJSON(manifest, pretty = TRUE)
}

getRepository <- function(description) {
  package <- description$Package
  priority <- description$Priority
  repository <- description$Repository
  githubRepo <- description$GithubRepo
  if (is.null(repository)) {
    if (identical(priority, "base") || identical(priority, "recommended"))
      repository <- "CRAN"
    else if ("biocViews" %in% names(description))
      repository <- "BioC"
    else if (!is.null(githubRepo))
      repository <- "GitHub"
    else if (package %in% .biocExtraPackages)
      repository <- "BioC"
  }
  repository
}

validateRepository <- function(pkg, repository) {
  msg <- if (is.null(repository)) {
    "The package was installed locally from source."
  } else if (!(repository %in% c("CRAN", "GitHub", "BioC"))) {
    paste(" The package was installed from an unsupported ",
          "repository '", repository, "'.", sep = "")
  }
  if (is.null(msg)) return()
  msg <- paste(
    "Unable to deploy package dependency '", pkg, "'\n\n", msg, " ",
    "Only packages installed from CRAN, BioConductor and GitHub are supported.\n",
    sep = ""
  )
  if (!hasRequiredDevtools()) {
    msg <- paste(msg, "\nTo use packages from GitHub you need to install ",
                 "them with the most recent version of devtools. ",
                 "To ensure you have the latest version of devtools ",
                 "use:\n\n",
                 "install.packages('devtools'); ",
                 "devtools::install_github('devtools')\n", sep = "")
  }
  msg
}

hasRequiredDevtools <- function() {
  "devtools" %in% .packages(all.available = TRUE) &&
  packageVersion("devtools") > "1.3"
}

addPackratSnapshot <- function(bundleDir, appMode) {
  # check for extra dependencies congruent to application mode
  extraPkgDeps <- ""
  if (grepl("\\brmd\\b", appMode))
    extraPkgDeps <- paste0(extraPkgDeps, "library(rmarkdown)\n")
  if (grepl("\\bshiny\\b", appMode))
    extraPkgDeps <- paste0(extraPkgDeps, "library(shiny)\n")

  # if we discovered any extra dependencies, write them to a file for packrat to
  # discover when it creates the snapshot
  tempDependencyFile <- file.path(bundleDir, "__rsconnect_deps.R")
  if (nchar(extraPkgDeps) > 0) {
    # emit dependencies to file
    writeLines(extraPkgDeps, tempDependencyFile)

    # ensure temp file is cleaned up even if there's an error
    on.exit({
      if (file.exists(tempDependencyFile))
        unlink(tempDependencyFile)
      }, add = TRUE)
  }

  # ensure we have an up-to-date packrat lockfile
  packratVersion <- packageVersion("packrat")
  requiredVersion <- "0.4.1.19"
  if (packratVersion < requiredVersion) {
    stop("rsconnect requires version '", requiredVersion, "' of Packrat; ",
         "you have version '", packratVersion, "' installed.\n",
         "Please use devtools::install_github('rstudio/packrat') to obtain ",
         "the latest version of Packrat.")
  }
  suppressMessages(
    packrat::.snapshotImpl(project = bundleDir,
                           snapshot.sources = FALSE,
                           verbose = FALSE)
  )

  # if we emitted a temporary dependency file for packrat's benefit, remove it
  # now so it isn't included in the bundle sent to the server
  if (file.exists(tempDependencyFile)) {
    unlink(tempDependencyFile)
  }
}

# given a list of mixed files and directories, explodes the directories
# recursively into their constituent files, and returns just a list of files
explodeFiles <- function(dir, files) {
  exploded <- c()
  for (f in files) {
    target <- file.path(dir, f)
    info <- file.info(target)
    if (is.na(info$isdir)) {
      # don't return this file; it doesn't appear to exist
      next
    } else if (isTRUE(info$isdir)) {
      # a directory; explode it
      contents <- list.files(target, full.names = FALSE, recursive = TRUE,
                             include.dirs = FALSE)
      exploded <- c(exploded, file.path(f, contents))
    } else {
      # not a directory; an ordinary file
      exploded <- c(exploded, f)
    }
  }
  exploded
}
