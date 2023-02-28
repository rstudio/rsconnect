# Given a path to an directory and a list of files in that directory, copies
 # those files to a new temporary directory. Performes some small modifications
 # in this process, including renaming single-file Shiny apps to "app.R" and
 # stripping packrat and renv commands from .Rprofile. Returns the path to the
 # temporary directory.
 bundleAppDir <- function(appDir, appFiles, appPrimaryDoc = NULL, verbose = FALSE) {

  logger <- verboseLogger(verbose)
  logger("Creating tempfile for appdir")
  # create a directory to stage the application bundle in
  bundleDir <- tempfile()
  dir.create(bundleDir, recursive = TRUE)
  on.exit(unlink(bundleDir), add = TRUE)

  logger("Copying files")
  # copy the files into the bundle dir
  for (file in appFiles) {
    logger("Copying", file)
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
    file.copy(from, to, copy.date = TRUE)

    # ensure .Rprofile doesn't call packrat/init.R or renv/activate.R
    if (basename(to) == ".Rprofile") {
      tweakRProfile(to)
    }

  }
  bundleDir
}

tweakRProfile <- function(path) {
  lines <- readLines(path)

  packratLines <- grep('source("packrat/init.R")', lines, fixed = TRUE)
  if (length(packratLines) > 0) {
    lines[packratLines] <- paste0(
       "# Packrat initialization disabled in published application\n",
       '# source("packrat/init.R")'
      )
  }

  renvLines <- grep('source("renv/activate.R")', lines, fixed = TRUE)
  if (length(renvLines) > 0) {
    lines[renvLines] <- paste0(
       "# renv initialization disabled in published application\n",
       '# source("renv/activate.R")'
      )
  }

  if (length(renvLines) > 0 || length(packratLines) > 0) {
    msg <-  sprintf(
      "# Modified by rsconnect package %s on %s",
      packageVersion("rsconnect"),
      Sys.time()
    )
    lines <- c(msg, lines)
  }

  writeLines(lines, path)
}

# Writes a tar.gz file located at bundlePath containing all files in bundleDir.
writeBundle <- function(bundleDir, bundlePath, verbose = FALSE) {
  logger <- verboseLogger(verbose)

  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir), add = TRUE)

  tarImplementation <- getTarImplementation()
  logger("Using tar: ", tarImplementation)

  if (tarImplementation == "internal") {
    detectLongNames(bundleDir)
  }

  utils::tar(bundlePath, files = NULL, compression = "gzip", tar = tarImplementation)
}


getTarImplementation <- function() {
  # Check the rsconnect.tar option first. If that is unset, check the
  # RSCONNECT_TAR environment var. If neither are set, use "internal".
  tarImplementation <- getOption("rsconnect.tar", default = NA)
  if (is.na(tarImplementation) || !nzchar(tarImplementation)) {
    tarImplementation <- Sys.getenv("RSCONNECT_TAR", unset = NA)
  }
  if (is.na(tarImplementation) || !nzchar(tarImplementation)) {
    tarImplementation <- "internal"
  }
  return(tarImplementation)
}


isWindows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}

createAppManifest <- function(appDir,
                              appMode,
                              contentCategory,
                              hasParameters,
                              appPrimaryDoc,
                              assetTypeName,
                              users,
                              pythonConfig = NULL,
                              documentsHavePython = FALSE,
                              retainPackratDirectory = TRUE,
                              quartoInfo = NULL,
                              isCloud = FALSE,
                              image = NULL,
                              verbose = FALSE) {

  # provide package entries for all dependencies
  packages <- bundlePackages(
    appDir = appDir,
    appMode = appMode,
    hasParameters = hasParameters,
    documentsHavePython = documentsHavePython,
    quartoInfo = quartoInfo,
    verbose = verbose
  )

  needsPython <- documentsHavePython ||
    "jupyter" %in% quartoInfo$engines ||
    "reticulate" %in% names(packages)
  if (needsPython && !is.null(pythonConfig)) {
    python <- pythonConfig(appDir)

    packageFile <- file.path(appDir, python$package_manager$package_file)
    writeLines(python$package_manager$contents, packageFile)
    python$package_manager$contents <- NULL
  } else {
    python <- NULL
  }

  if (!retainPackratDirectory) {
    # Optionally remove the packrat directory when it will not be included in
    # deployments, such as manifest-only deployments.
    unlink(file.path(appDir, "packrat"), recursive = TRUE)
  }

  # build the list of files to checksum
  files <- list.files(appDir, recursive = TRUE, all.files = TRUE,
                      full.names = FALSE)

  # provide checksums for all files
  filelist <- list()
  for (file in files) {
    filepath <- file.path(appDir, file)
    checksum <- list(checksum = fileMD5.as.string(filepath))
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
  manifest$locale <- getOption("rsconnect.locale", detectLocale())
  manifest$platform <- paste(R.Version()$major, R.Version()$minor, sep = ".")

  metadata <- list(appmode = appMode)

  # emit appropriate primary document information
  primaryDoc <- ifelse(is.null(appPrimaryDoc) ||
                         tolower(tools::file_ext(appPrimaryDoc)) == "r",
                       NA, appPrimaryDoc)
  metadata$primary_rmd <- ifelse(appMode %in% c("rmd-shiny", "rmd-static", "quarto-shiny", "quarto-static"), primaryDoc, NA)
  metadata$primary_html <- ifelse(appMode == "static", primaryDoc, NA)

  # emit content category (plots, etc)
  metadata$content_category <- ifelse(!is.null(contentCategory),
                                      contentCategory, NA)
  metadata$has_parameters <- hasParameters

  # add metadata
  manifest$metadata <- metadata

  # if there is a target image, attach it to the environment
  if (!is.null(image)) {
    manifest$environment <- list(image = image)
  }

  # indicate whether this is a quarto app/doc
  if (!is.null(quartoInfo) && !isCloud) {
    manifest$quarto <- quartoInfo
  }
  # if there is python info for reticulate or Quarto, attach it
  if (!is.null(python)) {
    manifest$python <- python
  }
  # if there are no packages set manifest$packages to NA (json null)
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

  manifest
}
