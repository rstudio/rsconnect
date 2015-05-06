

bundleApp <- function(appDir) {

  # create a directory to stage the application bundle in
  bundleDir <- tempfile()
  dir.create(bundleDir, recursive=TRUE)
  on.exit(unlink(bundleDir), add = TRUE)
   
  # if necessary write an index.htm for shinydoc deployments
  indexFiles <- writeRmdIndex(appDir)
  on.exit(unlink(indexFiles), add = TRUE)
  
  # determine the files that will be in the bundle (exclude shinyapps dir
  # as well as common hidden files)
  files <- list.files(appDir, recursive=TRUE, all.files=TRUE)
  files <- files[!grepl(glob2rx("shinyapps/*"), files)]
  files <- files[!grepl(glob2rx(".svn/*"), files)]
  files <- files[!grepl(glob2rx(".git/*"), files)]
  files <- files[!grepl(glob2rx(".Rproj.user/*"), files)]
  files <- files[!grepl(glob2rx("*.Rproj"), files)]
  files <- files[!grepl(glob2rx(".DS_Store"), files)]
  files <- files[!grepl(glob2rx(".gitignore"), files)]
  
  # copy the files into the bundle dir
  for (file in files) {
    from <- file.path(appDir, file)
    to <- file.path(bundleDir, file)
    if (!file.exists(dirname(to)))
      dir.create(dirname(to), recursive=TRUE)
    file.copy(from, to)
  }
  
  # get application users
  users <- suppressWarnings(authorizedUsers(appDir))
  
  # generate the manifest and write it into the bundle dir
  manifestJson <- enc2utf8(createAppManifest(appDir, files, users))
  writeLines(manifestJson, file.path(bundleDir, "manifest.json"), useBytes=TRUE)
  
  # create the bundle and return it's path
  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir), add = TRUE)
  bundlePath <- tempfile("shinyapps-bundle", fileext = ".tar.gz")
  utils::tar(bundlePath, files = ".", compression = "gzip")
  bundlePath
}

createAppManifest <- function(appDir, files, users) {
   
  # provide package entries for all dependencies
  packages <- list()
  # potential error messages
  msg      <- NULL
  for (pkg in dirDependencies(appDir)) {
    
    # get the description
    description <- list(description = suppressWarnings(utils::packageDescription(pkg)))
  
    # if description is NA, application dependency may not be installed 
    if (is.na(description)) {
      msg <- c(msg, paste("Application depends on package \"", pkg, "\" but it is not",
                          " installed. Please resolve before continuing.", sep = ""))
      next
    }
    
    # validate the repository (returns an error message if there is a problem)
    msg <- c(msg, validateRepository(pkg, getRepository(description[[1]])))
    
    # append the bioc version to any bioconductor packages
    # TODO: resolve against actual BioC repo a package was pulled from
    # (in case the user mixed and matched)
    if ("biocViews" %in% names(description$description)) {

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
  if (length(msg)) stop(paste(formatUL(msg, '\n*'), collapse = '\n'), call. = FALSE)

  # provide checksums for all files
  filelist <- list()
  for (file in files) {
    checksum <- list(checksum = digest::digest(file.path(appDir, file), 
                                               algo="md5", file=TRUE))
    filelist[[file]] <- I(checksum)
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
  manifest$locale <- getOption('shinyapps.locale', detectLocale())
  manifest$platform <- paste(R.Version()$major, R.Version()$minor, sep=".") 
  
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
  "devtools" %in% .packages(all.available=TRUE) && 
  packageVersion("devtools") > "1.3"
}

