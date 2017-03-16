bundleAppDir <- function(appDir, appFiles, appPrimaryDoc = NULL) {
  # create a directory to stage the application bundle in
  bundleDir <- tempfile()
  dir.create(bundleDir, recursive = TRUE)
  on.exit(unlink(bundleDir), add = TRUE)

  # copy the files into the bundle dir
  for (file in appFiles) {
    from <- file.path(appDir, file)
    to <- file.path(bundleDir, file)
    # if deploying a single-file Shiny application, name it "app.R" so it can
    # be run as an ordinary Shiny application
    if (is.character(appPrimaryDoc) &&
        tolower(tools::file_ext(appPrimaryDoc)) == "r" &&
        file == appPrimaryDoc) {
      to <- file.path(bundleDir, "app.R")
    }
    if (!file.exists(dirname(to)))
      dir.create(dirname(to), recursive = TRUE)
    file.copy(from, to)

    # ensure .Rprofile doesn't call packrat/init.R
    if (basename(to) == ".Rprofile") {
      origRprofile <- readLines(to)
      msg <- paste0("# Modified by rsconnect package ", packageVersion("rsconnect"), " on ", Sys.time(), ":")
      replacement <- paste(msg,
                           "# Packrat initialization disabled in published application",
                           '# source(\"packrat/init.R\")', sep="\n")
      newRprofile <- gsub( 'source(\"packrat/init.R\")',
                           replacement,
                           origRprofile, fixed = TRUE)
      cat(newRprofile, file=to, sep="\n")
    }

  }
  bundleDir
}

isKnitrCacheDir <- function(subdir, contents) {
  if (grepl("^.+_cache$", subdir)) {
    stem <- substr(subdir, 1, nchar(subdir) - nchar("_cache"))
    rmd <- paste0(stem, ".Rmd")
    tolower(rmd) %in% tolower(contents)
  } else {
    FALSE
  }
}

maxDirectoryList <- function(dir, parent, totalSize) {
  # generate a list of files at this level
  contents <- list.files(dir, recursive = FALSE, all.files = TRUE,
                         include.dirs = TRUE, no.. = TRUE, full.names = FALSE)

  # at the root level, exclude those with a forbidden extension
  if (nchar(parent) == 0) {
    contents <- contents[!grepl(glob2rx("*.Rproj"), contents)]
    contents <- contents[!grepl(glob2rx(".DS_Store"), contents)]
    contents <- contents[!grepl(glob2rx(".gitignore"), contents)]
    contents <- contents[!grepl(glob2rx(".Rhistory"), contents)]
  }

  # sum the size of the files in the directory
  info <- file.info(file.path(dir, contents))
  size <- sum(info$size)
  if (is.na(size))
    size <- 0
  totalSize <- totalSize + size
  subdirContents <- NULL

  # if we haven't exceeded the maximum size, check each subdirectory
  if (totalSize < getOption("rsconnect.max.bundle.size")) {
    subdirs <- contents[info$isdir]
    for (subdir in subdirs) {

      # ignore known directories from the root
      if (nchar(parent) == 0 && subdir %in% c(
           "rsconnect", "packrat", ".svn", ".git", ".Rproj.user"))
        next

      # ignore knitr _cache directories
      if (isKnitrCacheDir(subdir, contents))
        next

      # get the list of files in the subdirectory
      dirList <- maxDirectoryList(file.path(dir, subdir),
                                  if (nchar(parent) == 0) subdir
                                  else file.path(parent, subdir),
                                  totalSize)
      totalSize <- totalSize + dirList$size
      subdirContents <- append(subdirContents, dirList$contents)

      # abort if we've reached the maximum size
      if (totalSize > getOption("rsconnect.max.bundle.size"))
        break

      # abort if we've reached the maximum number of files
      if ((length(contents) + length(subdirContents)) >
          getOption("rsconnect.max.bundle.files"))
        break
    }
  }

  # return the new size and accumulated contents
  list(
    size = size,
    totalSize = totalSize,
    contents = append(if (nchar(parent) == 0) contents[!info$isdir]
                      else file.path(parent, contents[!info$isdir]),
                      subdirContents))
}

#' List Files to be Bundled
#'
#' Given a directory containing an application, returns the names of the files
#' to be bundled in the application.
#'
#' @param appDir Directory containing the application.
#'
#' @details This function computes results similar to a recursive directory
#' listing from \code{\link{list.files}}, with the following constraints:
#'
#' \enumerate{
#' \item{If the total size of the files exceeds the maximum bundle size, no
#'    more files are listed. The maximum bundle size is controlled by the
#'    \code{rsconnect.max.bundle.size} option.}
#' \item{If the total size number of files exceeds the maximum number to be
#'    bundled, no more files are listed. The maximum number of files in the
#'    bundle is controlled by the \code{rsconnect.max.bundle.files} option.}
#' \item{Certain files and folders that don't need to be bundled, such as
#'    those containing internal version control and RStudio state, are
#'    excluded.}
#' }
#'
#' @return Returns a list containing the following elements:
#'
#' \tabular{ll}{
#' \code{contents} \tab A list of the files to be bundled \cr
#' \code{totalSize} \tab The total size of the files \cr
#' }
#'
#' @export
listBundleFiles <- function(appDir) {
  maxDirectoryList(appDir, "", 0)
}

bundleFiles <- function(appDir) {
  files <- listBundleFiles(appDir)
  if (files$totalSize > getOption("rsconnect.max.bundle.size")) {
    stop("The directory", appDir, "cannot be deployed because it is too ",
         "large (the maximum size is", getOption("rsconnect.max.bundle.size"),
         "bytes). Remove some files or adjust the rsconnect.max.bundle.size ",
         "option.")
  } else if (length(files$contents) > getOption("rsconnect.max.bundle.files")) {
    stop("The directory", appDir, "cannot be deployed because it contains ",
         "too many files (the maximum number of files is ",
         getOption("rsconnect.max.bundle.files"), "). Remove some files or ",
         "adjust the rsconnect.max.bundle.files option.")
  }

  files$contents
}

bundleApp <- function(appName, appDir, appFiles, appPrimaryDoc, assetTypeName,
                      contentCategory) {

  # infer the mode of the application from its layout
  appMode <- inferAppMode(appDir, appPrimaryDoc, appFiles)
  hasParameters <- appHasParameters(appDir, appFiles, contentCategory)

  # copy files to bundle dir to stage
  bundleDir <- bundleAppDir(appDir, appFiles, appPrimaryDoc)

  # get application users (for non-document deployments)
  users <- NULL
  if (is.null(appPrimaryDoc)) {
    users <- suppressWarnings(authorizedUsers(appDir))
  }

  # generate the manifest and write it into the bundle dir
  manifestJson <- enc2utf8(createAppManifest(bundleDir, appMode,
                                             contentCategory, hasParameters,
                                             appPrimaryDoc,
                                             assetTypeName, users))
  writeLines(manifestJson, file.path(bundleDir, "manifest.json"),
             useBytes = TRUE)

  # if necessary write an index.htm for shinydoc deployments
  indexFiles <- writeRmdIndex(appName, bundleDir)
  on.exit(unlink(indexFiles), add = TRUE)

  # create the bundle and return its path
  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir), add = TRUE)
  bundlePath <- tempfile("rsconnect-bundle", fileext = ".tar.gz")
  utils::tar(bundlePath, files = ".", compression = "gzip", tar = "internal")
  bundlePath
}

yamlFromRmd <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  delim <- grep("^(---|\\.\\.\\.)\\s*$", lines)
  if (length(delim) >= 2) {
    # If at least two --- or ... lines were found...
    if (delim[[1]] == 1 || all(grepl("^\\s*$", lines[1:delim[[1]]]))) {
      # and the first is a ---
      if(grepl("^---\\s*$", lines[delim[[1]]])) {
        # ...and the first --- line is not preceded by non-whitespace...
        if (diff(delim[1:2]) > 1) {
          # ...and there is actually something between the two --- lines...
          yamlData <- paste(lines[(delim[[1]] + 1):(delim[[2]] - 1)],
                            collapse = "\n")
          return(yaml::yaml.load(yamlData))
        }
      }
    }
  }
  return(NULL)
}

appHasParameters <- function(appDir, files, contentCategory) {

  # sites don't ever have parameters
  if (identical(contentCategory, "site"))
    return(FALSE)

  rmdFiles <- grep("^[^/\\\\]+\\.rmd$", files, ignore.case = TRUE, perl = TRUE,
                   value = TRUE)
  if (length(rmdFiles) > 0) {
    for (rmdFile in rmdFiles) {
      filename <- file.path(appDir, rmdFile)
      yaml <- yamlFromRmd(filename)
      if (!is.null(yaml)) {
        params <- yaml[["params"]]
        # We don't care about deep parameter processing, only that they exist.
        return(!is.null(params) && length(params) > 0)
      }
    }
  }
  FALSE
}

isShinyRmd <- function(filename) {
  yaml <- yamlFromRmd(filename)
  if (!is.null(yaml)) {
    runtime <- yaml[["runtime"]]
    if (!is.null(runtime) && grepl('^shiny', runtime)) {
      # ...and "runtime: shiny", then it's a dynamic Rmd.
      return(TRUE)
    }
  }
  return(FALSE)
}

inferAppMode <- function(appDir, appPrimaryDoc, files) {
  # single-file Shiny application
  if (!is.null(appPrimaryDoc) &&
      tolower(tools::file_ext(appPrimaryDoc)) == "r") {
    return("shiny")
  }

  # shiny directory
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
      if (isShinyRmd(file.path(appDir, rmdFile))) {
        return("rmd-shiny")
      }
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

## check for extra dependencies congruent to application mode
inferDependencies <- function(appMode, hasParameters) {
  deps <- c()
  if (grepl("\\brmd\\b", appMode)) {
    if (hasParameters) {
      # An Rmd with parameters needs shiny to run the customization app.
      deps <- c(deps, "shiny")
    }
    deps <- c(deps, "rmarkdown")
  }
  if (grepl("\\bshiny\\b", appMode)) {
    deps <- c(deps, "shiny")
  }
  unique(deps)
}

createAppManifest <- function(appDir, appMode, contentCategory, hasParameters,
                              appPrimaryDoc, assetTypeName, users) {

  # provide package entries for all dependencies
  packages <- list()
  # potential error messages
  msg      <- NULL

  # get package dependencies for non-static content deployment
  if (!identical(appMode, "static")) {

    # detect dependencies including inferred dependences
    deps = snapshotDependencies(appDir, inferDependencies(appMode, hasParameters))

    # construct package list from dependencies
    for (i in seq.int(nrow(deps))) {
      name <- deps[i, "Package"]

      # get package info
      info <- as.list(deps[i, c('Source',
                                'Repository')])

      # include github package info
      info <- c(info, as.list(deps[i, grep('Github', colnames(deps), perl = TRUE, value = TRUE)]))

      # get package description
      # TODO: should we get description from packrat/desc folder?
      info$description = suppressWarnings(utils::packageDescription(name))

      # if description is NA, application dependency may not be installed
      if (is.na(info$description[1])) {
        msg <- c(msg, paste0(capitalize(assetTypeName), " depends on package \"",
                             name, "\" but it is not installed. Please resolve ",
                             "before continuing."))
        next
      }

      # validate package source (returns an error message if there is a problem)
      msg <- c(msg, validatePackageSource(deps[i, ]))

      # good to go
      packages[[name]] <- info
    }
  }
  if (length(msg)) stop(paste(formatUL(msg, '\n*'), collapse = '\n'), call. = FALSE)

  # build the list of files to checksum
  files <- list.files(appDir, recursive = TRUE, all.files = TRUE,
                      full.names = FALSE)

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
  manifest$locale <- getOption('rsconnect.locale', detectLocale())
  manifest$platform <- paste(R.Version()$major, R.Version()$minor, sep = ".")

  metadata <- list(appmode = appMode)

  # emit appropriate primary document information
  primaryDoc <- ifelse(is.null(appPrimaryDoc) ||
                         tolower(tools::file_ext(appPrimaryDoc)) == "r",
                       NA, appPrimaryDoc)
  metadata$primary_rmd <- ifelse(grepl("\\brmd\\b", appMode), primaryDoc, NA)
  metadata$primary_html <- ifelse(appMode == "static", primaryDoc, NA)

  # emit content category (plots, etc)
  metadata$content_category <- ifelse(!is.null(contentCategory),
                                      contentCategory, NA)
  metadata$has_parameters <- hasParameters

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

validatePackageSource <- function(pkg) {
  msg <- NULL
  if (!(pkg$Source %in% c("CRAN", "Bioconductor", "github"))) {
    if (is.null(pkg$Repository)) {
      msg <- paste("The package was installed from an unsupported ",
                   "source '", pkg$Source, "'.", sep = "")
    }
  }
  if (is.null(msg)) return()
  msg <- paste("Unable to deploy package dependency '", pkg$Package,
               "'\n\n", msg, " ", sep = "")
  msg
}

hasRequiredDevtools <- function() {
  "devtools" %in% .packages(all.available = TRUE) &&
    packageVersion("devtools") > "1.3"
}

snapshotLockFile <- function(appDir) {
  file.path(appDir, "packrat", "packrat.lock")
}

addPackratSnapshot <- function(bundleDir, implicit_dependencies = c()) {
  # if we discovered any extra dependencies, write them to a file for packrat to
  # discover when it creates the snapshot
  tempDependencyFile <- file.path(bundleDir, "__rsconnect_deps.R")
  if (length(implicit_dependencies) > 0) {
    extraPkgDeps <- paste0(lapply(implicit_dependencies,
                                  function(dep) {
                                    paste0("library(", dep, ")\n")
                                  }),
                           collapse="")
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
  requiredVersion <- "0.4.6"
  if (packratVersion < requiredVersion) {
    stop("rsconnect requires version '", requiredVersion, "' of Packrat; ",
         "you have version '", packratVersion, "' installed.\n",
         "Please install the latest version of Packrat from CRAN with:\n- ",
         "install.packages('packrat', type = 'source')")
  }

  # generate the packrat snapshot
  tryCatch({
    performPackratSnapshot(bundleDir)
  }, error = function(e) {
    # if an error occurs while generating the snapshot, add a header to the
    # message for improved attribution
    e$msg <- paste0("----- Error snapshotting dependencies (Packrat) -----\n",
                    e$msg)

    # print a traceback if enabled
    if (isTRUE(getOption("rsconnect.error.trace"))) {
      traceback(3, sys.calls())
    }

    # rethrow error so we still halt deployment
    stop(e)
  })

  # if we emitted a temporary dependency file for packrat's benefit, remove it
  # now so it isn't included in the bundle sent to the server
  if (file.exists(tempDependencyFile)) {
    unlink(tempDependencyFile)
  }

  # Copy all the DESCRIPTION files we're relying on into packrat/desc.
  # That directory will contain one file for each package, e.g.
  # packrat/desc/shiny will be the shiny package's DESCRIPTION.
  #
  # The server will use this to calculate package hashes. We don't want
  # to rely on hashes calculated by our version of packrat, because the
  # server may be running a different version.
  lockFilePath <- snapshotLockFile(bundleDir)
  descDir <- file.path(bundleDir, "packrat", "desc")
  tryCatch({
    dir.create(descDir)
    packages <- na.omit(read.dcf(lockFilePath)[,"Package"])
    lapply(packages, function(pkgName) {
      descFile <- system.file("DESCRIPTION", package = pkgName)
      if (!file.exists(descFile)) {
        stop("Couldn't find DESCRIPTION file for ", pkgName)
      }
      file.copy(descFile, file.path(descDir, pkgName))
    })
  }, error = function(e) {
    warning("Unable to package DESCRIPTION files: ", conditionMessage(e), call. = FALSE)
    if (dirExists(descDir)) {
      unlink(descDir, recursive = TRUE)
    }
  })

  invisible()
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

performPackratSnapshot <- function(bundleDir) {

  # move to the bundle directory
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(bundleDir)

  # ensure we snapshot recommended packages
  srp <- packrat::opts$snapshot.recommended.packages()
  packrat::opts$snapshot.recommended.packages(TRUE, persist = FALSE)
  on.exit(packrat::opts$snapshot.recommended.packages(srp, persist = FALSE),
          add = TRUE)

  # attempt to eagerly load the BiocInstaller package if installed, to work
  # around an issue where attempts to load the package could fail within a
  # 'suppressMessages()' context
  if (length(find.package("BiocInstaller", quiet = TRUE)))
    requireNamespace("BiocInstaller", quietly = TRUE)

  # generate a snapshot
  suppressMessages(
    packrat::.snapshotImpl(project = bundleDir,
                           snapshot.sources = FALSE,
                           verbose = FALSE)
  )

  # TRUE just to indicate success
  TRUE
}
