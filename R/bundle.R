bundleAppDir <- function(appDir, appFiles, appPrimaryDoc = NULL, verbose = FALSE) {
  if (verbose)
    timestampedLog("Creating tempfile for appdir")
  # create a directory to stage the application bundle in
  bundleDir <- tempfile()
  dir.create(bundleDir, recursive = TRUE)
  on.exit(unlink(bundleDir), add = TRUE)

  if (verbose)
    timestampedLog("Copying files")
  # copy the files into the bundle dir
  for (file in appFiles) {
    if (verbose)
      timestampedLog("Copying", file)
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

    # ensure .Rprofile doesn't call packrat/init.R or renv/activate.R
    if (basename(to) == ".Rprofile") {
      origRprofile <- readLines(to)
      msg <- paste0("# Modified by rsconnect package ", packageVersion("rsconnect"), " on ", Sys.time(), ":")

      packratReplacement <- paste(msg,
                                  "# Packrat initialization disabled in published application",
                                  '# source(\"packrat/init.R\")', sep="\n")
      renvReplacement <- paste(msg,
                               "# renv initialization disabled in published application",
                               '# source(\"renv/activate.R\")', sep="\n")
      newRprofile <- origRprofile
      newRprofile <- gsub('source(\"packrat/init.R\")',
                          packratReplacement,
                          newRprofile, fixed = TRUE)
      newRprofile <- gsub('source(\"renv/activate.R\")',
                          renvReplacement,
                          newRprofile, fixed = TRUE)
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

# dir is the path for this step on our recursive walk.
# depth is tracks the number of directories we have descended. depth==0 at the root.
# totalSize is a running total of our encountered file sizes.
# totalFiles is a running count of our encountered files.
maxDirectoryList <- function(dir, depth, totalFiles, totalSize) {
  # generate a list of files at this level
  contents <- list.files(dir, recursive = FALSE, all.files = TRUE,
                         include.dirs = TRUE, no.. = TRUE, full.names = FALSE)

  # At the root, some well-known files and directories are not included in the bundle.
  if (depth==0) {
    contents <- contents[!grepl(glob2rx("*.Rproj"), contents)]
    contents <- setdiff(contents, c(
                                      ".DS_Store",
                                      ".gitignore",
                                      ".Rhistory",
                                      "manifest.json",
                                      "rsconnect",
                                      "packrat",
                                      "app_cache",
                                      ".svn",
                                      ".git",
                                      ".Rproj.user"
                                  ))
  }

  # exclude renv files
  contents <- setdiff(contents, c("renv", "renv.lock"))


  # checks for .rscignore file and excludes the files and directories listed
  if (".rscignore" %in% contents) {
    ignoreContents <- readLines(".rscignore")
    contents <- setdiff(
      x = contents,
      y = ignoreContents
    )
  }

  # subdirContents contains all files encountered beneath this directory.
  # Returned paths are relative to this directory.
  subdirContents <- NULL

  # Info for each file lets us know to recurse (directories) or aggregate (files).
  infos <- file.info(file.path(dir, contents), extra_cols = FALSE)
  row.names(infos) <- contents

  for (name in contents) {
    info <- infos[name,]

    if (info$isdir) {
      # Directories do not include their self-size in our counts.

      # ignore knitr _cache directories
      if (isKnitrCacheDir(name, contents)) {
        next
      }

      # Recursively enumerate this directory.
      dirList <- maxDirectoryList(file.path(dir, name), depth+1, totalFiles, totalSize)

      # Inherit the running totals from our child.
      totalSize <- dirList$totalSize
      totalFiles <- dirList$totalFiles

      # Directories are not included, only their files.
      subdirContents <- append(subdirContents, file.path(name, dirList$contents))

    } else {
      # This is a file. It counts and is included in our listing.

      totalSize <- totalSize + info$size
      totalFiles <- totalFiles + 1
      subdirContents <- append(subdirContents, name)
    }

    # abort if we've reached the maximum size
    if (totalSize > getOption("rsconnect.max.bundle.size"))
      break

    # abort if we've reached the maximum number of files
    if (totalFiles > getOption("rsconnect.max.bundle.files"))
      break
  }

  # totalSize - incoming size summed with all file sizes beneath this directory.
  # totalFiles - incoming count summed with file count beneath this directory.
  # contents - all files beneath this directory; paths relative to this directory.
  list(
      totalSize = totalSize,
      totalFiles = totalFiles,
      contents = subdirContents
  )
}

#' List Files to be Bundled
#'
#' Given a directory containing an application, returns the names of the files
#' to be bundled in the application.
#'
#' @param appDir Directory containing the application.
#'
#' @details This function computes results similar to a recursive directory
#' listing from [list.files()], with the following constraints:
#'
#' \enumerate{
#' \item{If the total size of the files exceeds the maximum bundle size, no
#'    more files are listed. The maximum bundle size is controlled by the
#'    `rsconnect.max.bundle.size` option.}
#' \item{If the total size number of files exceeds the maximum number to be
#'    bundled, no more files are listed. The maximum number of files in the
#'    bundle is controlled by the `rsconnect.max.bundle.files` option.}
#' \item{Certain files and folders that don't need to be bundled, such as
#'    those containing internal version control and RStudio state, are
#'    excluded.}
#' }
#'
#' @return Returns a list containing the following elements:
#'
#' \tabular{ll}{
#' `contents` \tab A list of the files to be bundled \cr
#' `totalSize` \tab The total size of the files \cr
#' }
#'
#' @export
listBundleFiles <- function(appDir) {
  maxDirectoryList(appDir, 0, 0, 0)
}

bundleFiles <- function(appDir) {
  files <- listBundleFiles(appDir)
  if (files$totalSize > getOption("rsconnect.max.bundle.size")) {
    stop("The directory ", appDir, " cannot be deployed because it is too ",
         "large (the maximum size is ", getOption("rsconnect.max.bundle.size"),
         " bytes). Remove some files or adjust the rsconnect.max.bundle.size ",
         "option.")
  } else if (length(files$contents) > getOption("rsconnect.max.bundle.files")) {
    stop("The directory ", appDir, " cannot be deployed because it contains ",
         "too many files (the maximum number of files is ",
         getOption("rsconnect.max.bundle.files"), "). Remove some files or ",
         "adjust the rsconnect.max.bundle.files option.")
  }

  files$contents
}

bundleApp <- function(appName, appDir, appFiles, appPrimaryDoc, assetTypeName,
                      contentCategory, verbose = FALSE, python = NULL,
                      condaMode = FALSE, forceGenerate = FALSE) {
  logger <- verboseLogger(verbose)

  logger("Inferring App mode and parameters")
  appMode <- inferAppMode(
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc,
      files = appFiles)
  appPrimaryDoc <- inferAppPrimaryDoc(
      appPrimaryDoc = appPrimaryDoc,
      appFiles = appFiles,
      appMode = appMode)
  hasParameters <- appHasParameters(
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc,
      appMode = appMode,
      contentCategory = contentCategory)
  hasPythonRmd <- appHasPythonRmd(
      appDir = appDir,
      files = appFiles)

  # get application users (for non-document deployments)
  users <- NULL
  if (is.null(appPrimaryDoc)) {
    users <- suppressWarnings(authorizedUsers(appDir))
  }

  # copy files to bundle dir to stage
  logger("Bundling app dir")
  bundleDir <- bundleAppDir(
      appDir = appDir,
      appFiles = appFiles,
      appPrimaryDoc = appPrimaryDoc)
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)

  # generate the manifest and write it into the bundle dir
  logger("Generate manifest.json")
  manifest <- createAppManifest(
      appDir = bundleDir,
      appMode = appMode,
      contentCategory = contentCategory,
      hasParameters = hasParameters,
      appPrimaryDoc = appPrimaryDoc,
      assetTypeName = assetTypeName,
      users = users,
      condaMode = condaMode,
      forceGenerate = forceGenerate,
      python = python,
      hasPythonRmd = hasPythonRmd,
      retainPackratDirectory = TRUE)
  manifestJson <- enc2utf8(toJSON(manifest, pretty = TRUE))
  manifestPath <- file.path(bundleDir, "manifest.json")
  writeLines(manifestJson, manifestPath, useBytes = TRUE)

  # if necessary write an index.htm for shinydoc deployments
  logger("Writing Rmd index if necessary")
  indexFiles <- writeRmdIndex(appName, bundleDir)

  # create the bundle and return its path
  logger("Compressing the bundle")
  bundlePath <- tempfile("rsconnect-bundle", fileext = ".tar.gz")
  writeBundle(bundleDir, bundlePath)
  bundlePath
}

# Writes a tar.gz file located at bundlePath containing all files in bundleDir.
writeBundle <- function(bundleDir, bundlePath, verbose = FALSE) {
  logger <- verboseLogger(verbose)

  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir), add = TRUE)

  tarImplementation <- Sys.getenv("RSCONNECT_TAR", "internal")
  logger(sprintf("Using tar: %s", tarImplementation))

  if (tarImplementation == "internal") {
    detectLongNames(bundleDir)
  }

  utils::tar(bundlePath, files = NULL, compression = "gzip", tar = tarImplementation)
}

# uname/grname is not always available.
# https://github.com/wch/r-source/blob/8cf68878a1361d00ff2125db2e1ac7dc8f6c8009/src/library/utils/R/tar.R#L539-L549
longerThan <- function(s, lim) {
  if (!is.null(s) && !is.na(s)) {
    return(nchar(s) > lim)
  }
  return(FALSE)
}

# Scan the bundle directory looking for long user/group names.
#
# Warn that the internal tar implementation may produce invalid archives.
# https://github.com/rstudio/rsconnect/issues/446
# https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17871
detectLongNames <- function(bundleDir, lengthLimit = 32) {
  files <- list.files(bundleDir, recursive = TRUE, all.files = TRUE,
                      include.dirs = TRUE, no.. = TRUE, full.names = FALSE)
  for (f in files) {
    info <- file.info(file.path(bundleDir,f))


    if (longerThan(info$uname, lengthLimit) || longerThan(info$grname, lengthLimit)) {
      warning("The bundle contains files with user/group names having more than ", lengthLimit,
              " characters: ", f, " is owned by ", info$uname, ":", info$grname, ". ",
              "Long user and group names cause the internal R tar implementation to produce invalid archives. ",
              "Set the RSCONNECT_TAR environment variable to use an external tar command.")
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}


#' Create a manifest.json describing deployment requirements.
#'
#' Given a directory content targeted for deployment, write a manifest.json
#' into that directory describing the deployment requirements for that
#' content.
#'
#' @param appDir Directory containing the content (Shiny application, R
#'   Markdown document, etc).
#'
#' @param appFiles Optional. The full set of files and directories to be
#'   included in future deployments of this content. Used when computing
#'   dependency requirements. When `NULL`, all files in `appDir` are
#'   considered.
#'
#' @param appPrimaryDoc Optional. Specifies the primary document in a content
#'   directory containing more than one. If `NULL`, the primary document is
#'   inferred from the file list.
#'
#' @param contentCategory Optional. Specifies the kind of content being
#'   deployed (e.g. `"plot"` or `"site"`).
#'
#' @param python Full path to a python binary for use by `reticulate`.
#'   The specified python binary will be invoked to determine its version
#'   and to list the python packages installed in the environment.
#'   If python = NULL, and RETICULATE_PYTHON is set in the environment,
#'   its value will be used.
#'
#' @param forceGeneratePythonEnvironment Optional. If an existing
#'   `requirements.txt` file is found, it will be overwritten when
#'   this argument is `TRUE`.
#'
#'
#' @export
writeManifest <- function(appDir = getwd(),
                          appFiles = NULL,
                          appPrimaryDoc = NULL,
                          contentCategory = NULL,
                          python = NULL,
                          forceGeneratePythonEnvironment = FALSE) {

  condaMode <- FALSE

  if (is.null(appFiles)) {
    appFiles <- bundleFiles(appDir)
  } else {
    appFiles <- explodeFiles(appDir, appFiles)
  }

  appMode <- inferAppMode(
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc,
      files = appFiles)
  appPrimaryDoc <- inferAppPrimaryDoc(
      appPrimaryDoc = appPrimaryDoc,
      appFiles = appFiles,
      appMode = appMode)
  hasParameters <- appHasParameters(
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc,
      appMode = appMode,
      contentCategory = contentCategory)
  hasPythonRmd <- appHasPythonRmd(
      appDir = appDir,
      files = appFiles)

  # copy files to bundle dir to stage
  bundleDir <- bundleAppDir(
      appDir = appDir,
      appFiles = appFiles,
      appPrimaryDoc = appPrimaryDoc)
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)

  python <- getPython(python)

  # generate the manifest and write it into the bundle dir
  manifest <- createAppManifest(
      appDir = bundleDir,
      appMode = appMode,
      contentCategory = contentCategory,
      hasParameters = hasParameters,
      appPrimaryDoc = appPrimaryDoc,
      assetTypeName = "content",
      users = NULL,
      condaMode = condaMode,
      forceGenerate = forceGeneratePythonEnvironment,
      python = python,
      hasPythonRmd = hasPythonRmd,
      retainPackratDirectory = FALSE)

  manifestJson <- enc2utf8(toJSON(manifest, pretty = TRUE))
  manifestPath <- file.path(appDir, "manifest.json")
  writeLines(manifestJson, manifestPath, useBytes = TRUE)

  requirementsFilename <- manifest$python$package_manager$package_file
  if (is.null(requirementsFilename)) { requirementsFilename <- "requirements.txt" }
  srcRequirementsFile <- file.path(bundleDir, requirementsFilename)
  dstRequirementsFile <- file.path(appDir, requirementsFilename)
  if(file.exists(srcRequirementsFile) && !file.exists(dstRequirementsFile)) {
    file.copy(srcRequirementsFile, dstRequirementsFile)
  }
  invisible()
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

rmdHasPythonBlock <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  matches <- grep("`{python", lines, fixed = TRUE)
  return (length(matches) > 0)
}

appHasPythonRmd <- function(appDir, files) {
  rmdFiles <- grep("^[^/\\\\]+\\.rmd$", files, ignore.case = TRUE, perl = TRUE,
                   value = TRUE)

  if (length(rmdFiles) > 0) {
    for (rmdFile in rmdFiles) {
      if (rmdHasPythonBlock(file.path(appDir, rmdFile))) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

appHasParameters <- function(appDir, appPrimaryDoc, appMode, contentCategory) {
  # Only Rmd deployments are marked as having parameters. Shiny applications
  # may distribute an Rmd alongside app.R, but that does not cause the
  # deployment to be considered parameterized.
  #
  # https://github.com/rstudio/rsconnect/issues/246
  if (!(appMode %in% c("rmd-static", "rmd-shiny"))) {
    return(FALSE)
  }
  # Sites don't ever have parameters
  if (identical(contentCategory, "site")) {
    return(FALSE)
  }

  # Only Rmd files have parameters.
  if (tolower(tools::file_ext(appPrimaryDoc)) == "rmd") {
    filename <- file.path(appDir, appPrimaryDoc)
    yaml <- yamlFromRmd(filename)
    if (!is.null(yaml)) {
      params <- yaml[["params"]]
      # We don't care about deep parameter processing, only that they exist.
      return(!is.null(params) && length(params) > 0)
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

# infer the mode of the application from its layout
# unless we're an API, in which case, we're API mode.
inferAppMode <- function(appDir, appPrimaryDoc, files) {
  # plumber API
  plumberFiles <- grep("^(plumber|entrypoint).r$", files, ignore.case = TRUE, perl = TRUE)
  if (length(plumberFiles) > 0) {
    return("api")
  }

  # single-file Shiny application
  if (!is.null(appPrimaryDoc) &&
      tolower(tools::file_ext(appPrimaryDoc)) == "r") {
    return("shiny")
  }

  # Shiny application using single-file app.R style.
  appR <- grep("^app.r$", files, ignore.case = TRUE, perl = TRUE)
  if (length(appR) > 0) {
    return("shiny")
  }

  # Determine if we have Rmd and if they are (optionally) need the Shiny runtime.
  rmdFiles <- grep("^[^/\\\\]+\\.rmd$", files, ignore.case = TRUE, perl = TRUE, value = TRUE)
  shinyRmdFiles <- sapply(file.path(appDir, rmdFiles), isShinyRmd)

  # An Rmd file with a Shiny runtime uses rmarkdown::run.
  if (any(shinyRmdFiles)) {
    return("rmd-shiny")
  }

  # Shiny application using server.R; checked later than Rmd with shiny runtime
  # because server.R may contain the server code paired with a ShinyRmd and needs
  # to be run by rmarkdown::run (rmd-shiny).
  serverR <- grep("^server.r$", files, ignore.case = TRUE, perl = TRUE)
  if (length(serverR) > 0) {
    return("shiny")
  }

  # Any non-Shiny R Markdown documents are rendered content (rmd-static).
  if (length(rmdFiles) > 0) {
    return("rmd-static")
  }

  # We don't have an RMarkdown, Shiny app, or Plumber API, but we have a saved model
  if(length(grep("(saved_model.pb|saved_model.pbtxt)$", files, ignore.case = TRUE, perl = TRUE)) > 0) {
    return("tensorflow-saved-model")
  }

  # no renderable content here; if there's at least one file, we can just serve
  # it as static content
  if (length(files) > 0) {
    return("static")
  }

  # there doesn't appear to be any content here we can use
  return(NA)
}

inferAppPrimaryDoc <- function(appPrimaryDoc, appFiles, appMode) {
  # if deploying an R Markdown app or static content, infer a primary document
  # if not already specified
  if ((grepl("rmd", appMode, fixed = TRUE) || appMode == "static")
      && is.null(appPrimaryDoc)) {
    # determine expected primary document extension
    ext <- ifelse(appMode == "static", "html?", "Rmd")

    # use index file if it exists
    primary <- which(grepl(paste0("^index\\.", ext, "$"), appFiles, fixed = FALSE,
                           ignore.case = TRUE))
    if (length(primary) == 0) {
      # no index file found, so pick the first one we find
      primary <- which(grepl(paste0("^.*\\.", ext, "$"), appFiles, fixed = FALSE,
                             ignore.case = TRUE))
      if (length(primary) == 0) {
        stop("Application mode ", appMode, " requires at least one document.")
      }
    }
    # if we have multiple matches, pick the first
    if (length(primary) > 1)
      primary <- primary[[1]]
    appPrimaryDoc <- appFiles[[primary]]
  }
  appPrimaryDoc
}

## check for extra dependencies congruent to application mode
inferDependencies <- function(appMode, hasParameters, python, hasPythonRmd) {
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
  if (appMode == 'api') {
    deps <- c(deps, "plumber")
  }
  if (hasPythonRmd) {
    deps <- c(deps, "reticulate")
  }
  unique(deps)
}

isWindows <- function() {
  Sys.info()[["sysname"]] == "Windows"
}

getCondaEnvPrefix <- function(python) {
  prefix <- dirname(dirname(python))
  if (!file.exists(file.path(prefix, "conda-meta"))) {
    stop(paste("Python from", python, "does not look like a conda environment: cannot find `conda-meta`"))
  }
  prefix
}

getCondaExeForPrefix <- function(prefix) {
  miniconda <- dirname(dirname(prefix))
  conda <- file.path(miniconda, 'bin', 'conda')
  if (isWindows()) {
    conda <- paste(conda, ".exe", sep = "")
  }
  if (!file.exists(conda)) {
    stop(paste("Conda env prefix", prefix, "does not have the `conda` command line interface."))
  }
  conda
}

inferPythonEnv <- function(workdir, python, condaMode, forceGenerate) {
  # run the python introspection script
  env_py <- system.file("resources/environment.py", package = "rsconnect")
  args <- c(shQuote(env_py))
  if (condaMode || forceGenerate) {
    flags <- paste('-', ifelse(condaMode, 'c', ''), ifelse(forceGenerate, 'f', ''), sep = '')
    args <- c(args, flags)
  }
  args <- c(args, shQuote(workdir))

  tryCatch({
    # First check for reticulate. Then see if python is loaded in reticulate space, verify anaconda presence,
    # and verify that the user hasn't specified that they don't want their conda environment captured.
    if('reticulate' %in% rownames(installed.packages()) && reticulate::py_available(initialize = FALSE) &&
       reticulate::py_config()$anaconda && !condaMode) {
      prefix <- getCondaEnvPrefix(python)
      conda <- getCondaExeForPrefix(prefix)
      args <- c("run", "-p", prefix, python, args)
      # conda run -p <prefix> python inst/resources/environment.py <flags> <dir>
      output <- system2(command = conda, args = args, stdout = TRUE, stderr = NULL, wait = TRUE)
    } else {
      output <- system2(command = python, args = args, stdout = TRUE, stderr = NULL, wait = TRUE)
    }
    environment <- jsonlite::fromJSON(output)
    if (is.null(environment$error)) {
      list(
          version = environment$python,
          package_manager = list(
              name = environment$package_manager,
              version = environment[[environment$package_manager]],
              package_file = environment$filename,
              contents = environment$contents))
    }
    else {
      # return the error
      environment
    }
  }, error = function(e) {
    list(error = e$message)
  })
}

createAppManifest <- function(appDir, appMode, contentCategory, hasParameters,
                              appPrimaryDoc, assetTypeName, users, condaMode,
                              forceGenerate, python = NULL, hasPythonRmd = FALSE,
                              retainPackratDirectory = TRUE) {

  # provide package entries for all dependencies
  packages <- list()
  # non-SCM repository sources without URLs
  missing_url_sources <- NULL
  # potential error messages
  msg      <- NULL
  pyInfo   <- NULL

  # get package dependencies for non-static content deployment
  if (!identical(appMode, "static") &&
      !identical(appMode, "tensorflow-saved-model")) {

    # detect dependencies including inferred dependences
    deps = snapshotDependencies(appDir, inferDependencies(appMode, hasParameters, python, hasPythonRmd))

    # construct package list from dependencies
    for (i in seq.int(nrow(deps))) {
      name <- deps[i, "Package"]

      if (name == "reticulate" && !is.null(python)) {
        pyInfo <- inferPythonEnv(appDir, python, condaMode, forceGenerate)
        if (is.null(pyInfo$error)) {
          # write the package list into requirements.txt/environment.yml file in the bundle dir
          packageFile <- file.path(appDir, pyInfo$package_manager$package_file)
          cat(pyInfo$package_manager$contents, file=packageFile, sep="\n")
          pyInfo$package_manager$contents <- NULL
        }
        else {
          msg <- c(msg, paste("Error detecting python for reticulate:", pyInfo$error))
        }
      }

      # get package info
      info <- as.list(deps[i, c('Source',
                                'Repository')])

      if (is.na(info$Repository)) {
        if (isSCMSource(info$Source)) {
          # ignore source+SCM packages
        } else {
          missing_url_sources <- unique(c(missing_url_sources, info$Source))
        }
      }

      # include github package info
      info <- c(info, as.list(deps[i, grep('Github', colnames(deps), perl = TRUE, value = TRUE)]))

      # get package description; note that we need to remove the
      # packageDescription S3 class from the object or jsonlite will refuse to
      # serialize it when building the manifest JSON
      # TODO: should we get description from packrat/desc folder?
      info$description = suppressWarnings(unclass(utils::packageDescription(name)))

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
  if (length(missing_url_sources)) {
    # Err when packages lack repository URL. We emit a warning about each package (see
    # snapshotDependencies) before issuing an error with this resolution advice.
    #
    # It's possible we cannot find a repository URL for other reasons, including when folks locally
    # build and install packages from source. An incorrectly configured "repos" option is almost
    # always the cause.
    msg <- c(msg, sprintf("Unable to determine the location for some packages. Packages must come from a package repository like CRAN or a source control system. Check that options('repos') refers to a package repository containing the needed package versions."))
  }

  if (length(msg)) stop(paste(formatUL(msg, '\n*'), collapse = '\n'), call. = FALSE)

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

  # if there is python info for reticulate, attach it
  if (!is.null(pyInfo)) {
    manifest$python <- pyInfo
  }
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
  manifest
}

validatePackageSource <- function(pkg) {
  msg <- NULL
  if (!(pkg$Source %in% c("CRAN", "Bioconductor", "github", "gitlab", "bitbucket"))) {
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

  # attempt to eagerly load the BiocInstaller or BiocManaager package if installed, to work around
  # an issue where attempts to load the package could fail within a 'suppressMessages()' context
  packages <- c("BiocManager", "BiocInstaller")
  for (package in packages) {
    if (length(find.package(package, quiet = TRUE))) {
      requireNamespace(package, quietly = TRUE)
      break
    }
  }

  # generate a snapshot
  suppressMessages(
    packrat::.snapshotImpl(project = bundleDir,
                           snapshot.sources = FALSE,
                           fallback.ok = TRUE,
                           verbose = FALSE,
                           implicit.packrat.dependency = FALSE)
  )

  # TRUE just to indicate success
  TRUE
}
