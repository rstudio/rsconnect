# Given a path to an directory and a list of files in that directory, copies
 # those files to a new temporary directory. Performes some small modifications
 # in this process, including renaming single-file Shiny apps to "app.R" and
 # stripping packrat and renv commands from .Rprofile. Returns the path to the
 # temporary directory.
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
    file.copy(from, to, copy.date = TRUE)

    # ensure .Rprofile doesn't call packrat/init.R or renv/activate.R
    if (basename(to) == ".Rprofile") {
      origRprofile <- readLines(to)
      msg <- paste0("# Modified by rsconnect package ", packageVersion("rsconnect"), " on ", Sys.time(), ":")

      packratReplacement <- paste(msg,
                                  "# Packrat initialization disabled in published application",
                                  '# source(\"packrat/init.R\")', sep = "\n")
      renvReplacement <- paste(msg,
                               "# renv initialization disabled in published application",
                               '# source(\"renv/activate.R\")', sep = "\n")
      newRprofile <- origRprofile
      newRprofile <- gsub('source(\"packrat/init.R\")',
                          packratReplacement,
                          newRprofile, fixed = TRUE)
      newRprofile <- gsub('source(\"renv/activate.R\")',
                          renvReplacement,
                          newRprofile, fixed = TRUE)
      cat(newRprofile, file = to, sep = "\n")
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
  if (depth == 0) {
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
                                      ".quarto",
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
    info <- infos[name, ]

    if (isTRUE(info$isdir)) {
      # Directories do not include their self-size in our counts.

      # ignore knitr _cache directories
      if (isKnitrCacheDir(name, contents)) {
        next
      }

      # Recursively enumerate this directory.
      dirList <- maxDirectoryList(file.path(dir, name), depth + 1, totalFiles, totalSize)

      # Inherit the running totals from our child.
      totalSize <- dirList$totalSize
      totalFiles <- dirList$totalFiles

      # Directories are not included, only their files.
      subdirContents <- append(subdirContents, file.path(name, dirList$contents))

    } else {
      # This is a file. It counts and is included in our listing.
      if (is.na(info$isdir)) {
        cat(sprintf("File information for %s is not available; listing as a normal file.\n",
                    file.path(dir, name)))
      }

      ourSize <- if (is.na(info$size)) { 0 } else { info$size }
      totalSize <- totalSize + ourSize
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
#' \item{In order to stop specific files in the working directory from being
#'    listed in the bundle, the files must be listed in the .rscignore file.
#'    This file must have one file or directory per line with no support for
#'    wildcards.}
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
  enforceBundleLimits(appDir, files$totalSize, length(files$contents))
  files$contents
}

enforceBundleLimits <- function(appDir, totalSize, totalFiles) {
  if (totalSize > getOption("rsconnect.max.bundle.size")) {
    stop("The directory ", appDir, " cannot be deployed because it is too ",
         "large (the maximum size is ", getOption("rsconnect.max.bundle.size"),
         " bytes). Remove some files or adjust the rsconnect.max.bundle.size ",
         "option.", call. = FALSE)
  } else if (totalFiles > getOption("rsconnect.max.bundle.files")) {
    stop("The directory ", appDir, " cannot be deployed because it contains ",
         "too many files (the maximum number of files is ",
         getOption("rsconnect.max.bundle.files"), "). Remove some files or ",
         "adjust the rsconnect.max.bundle.files option.", call. = TRUE)
  }
}



# Writes a tar.gz file located at bundlePath containing all files in bundleDir.
writeBundle <- function(bundleDir, bundlePath, verbose = FALSE) {
  logger <- verboseLogger(verbose)

  prevDir <- setwd(bundleDir)
  on.exit(setwd(prevDir), add = TRUE)

  tarImplementation <- getTarImplementation()
  logger(sprintf("Using tar: %s", tarImplementation))

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
    info <- file.info(file.path(bundleDir, f))


    if (longerThan(info$uname, lengthLimit) || longerThan(info$grname, lengthLimit)) {
      warning("The bundle contains files with user/group names having more than ", lengthLimit,
              " characters: ", f, " is owned by ", info$uname, ":", info$grname, ". ",
              "Long user and group names cause the internal R tar implementation to produce invalid archives. ",
              "Set the rsconnect.tar option or the RSCONNECT_TAR environment variable to the path to ",
              "a tar executable to use that implementation.")
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}


yamlFromRmd <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  delim <- grep("^(---|\\.\\.\\.)\\s*$", lines)
  if (length(delim) >= 2) {
    # If at least two --- or ... lines were found...
    if (delim[[1]] == 1 || all(grepl("^\\s*$", lines[1:delim[[1]]]))) {
      # and the first is a ---
      if (grepl("^---\\s*$", lines[delim[[1]]])) {
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

documentHasPythonChunk <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  matches <- grep("`{python", lines, fixed = TRUE)
  return(length(matches) > 0)
}

detectPythonInDocuments <- function(appDir, files) {
  rmdFiles <- grep("^[^/\\\\]+\\.[rq]md$", files, ignore.case = TRUE, perl = TRUE,
                   value = TRUE)

  if (length(rmdFiles) > 0) {
    for (rmdFile in rmdFiles) {
      if (documentHasPythonChunk(file.path(appDir, rmdFile))) {
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
  parameterAppModes <- c(
      "rmd-static",
      "rmd-shiny",
      "quarto-static",
      "quarto-shiny"
  )
  if (!(appMode %in% parameterAppModes)) {
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
    server <- yaml[["server"]]
    if (!is.null(runtime) && grepl("^shiny", runtime)) {
      # ...and "runtime: shiny", then it's a dynamic Rmd.
      return(TRUE)
    } else if (!is.null(server)) {
      if (identical(server, "shiny")) {
        return(TRUE)
      } else if (is.list(server) && identical(server[["type"]], "shiny")) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# infer the mode of the application from its layout
# unless we're an API, in which case, we're API mode.
inferAppMode <- function(appDir, appPrimaryDoc, files, quartoInfo, isCloudServer = FALSE) {
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

  # Determine if we have Rmd files, and if they use the Shiny runtime.
  rmdFiles <- grep("^[^/\\\\]+\\.rmd$", files, ignore.case = TRUE, perl = TRUE, value = TRUE)
  shinyRmdFiles <- sapply(file.path(appDir, rmdFiles), isShinyRmd)

  # Determine if we have qmd files, and if they use the Shiny runtime
  qmdFiles <- grep("^[^/\\\\]+\\.qmd$", files, ignore.case = TRUE, perl = TRUE, value = TRUE)
  shinyQmdFiles <- sapply(file.path(appDir, qmdFiles), isShinyRmd)

  # We make Quarto requirement conditional on the presence of files that Quarto
  # can render and _quarto.yml, because keying off the presence of qmds
  # *or* _quarto.yml was causing deployment failures in static content.
  # https://github.com/rstudio/rstudio/issues/11444
  hasQuartoYaml <- any(grepl("^_quarto.y(a)?ml$", x = files, ignore.case = TRUE, perl = TRUE))
  hasQuartoCompatibleFiles <- any(length(qmdFiles) > 0, length(rmdFiles > 0))
  requiresQuarto <- (hasQuartoCompatibleFiles && hasQuartoYaml) || length(qmdFiles) > 0

  # We gate the deployment of content that appears to be Quarto behind the
  # presence of Quarto metadata. Rmd files can still be deployed as Quarto
  # content.
  if (requiresQuarto && is.null(quartoInfo)) {
    stop(paste(
      "Attempting to deploy Quarto content without Quarto metadata.",
      "Please provide the path to a quarto binary to the 'quarto' argument."
    ))
  }

  # Shiny or Quarto documents with "server: shiny" in their YAML front matter
  # are rmd-shiny or quarto-shiny.
  if (any(shinyRmdFiles) || any(shinyQmdFiles)) {
    if (!is.null(quartoInfo)) {
      return("quarto-shiny")
    } else {
      return("rmd-shiny")
    }
  }

  # Shiny application using server.R; checked later than Rmd with shiny runtime
  # because server.R may contain the server code paired with a ShinyRmd and needs
  # to be run by rmarkdown::run (rmd-shiny).
  serverR <- grep("^server.r$", files, ignore.case = TRUE, perl = TRUE)
  if (length(serverR) > 0) {
    return("shiny")
  }

  # Any non-Shiny R Markdown or Quarto documents are rendered content and get
  # rmd-static or quarto-static.
  if (length(rmdFiles) > 0 || length(qmdFiles) > 0) {
    if (!is.null(quartoInfo)) {
      return("quarto-static")
    } else {
      # For Shinyapps and posit.cloud, treat "rmd-static" app mode as "rmd-shiny" so that
      # they can be served from a shiny process in Connect until we have better support of
      # rmarkdown static content
      if (isCloudServer) {
        return("rmd-shiny")
      }
      return("rmd-static")
    }
  }

  # We don't have an RMarkdown, Shiny app, or Plumber API, but we have a saved model
  if (length(grep("(saved_model.pb|saved_model.pbtxt)$", files, ignore.case = TRUE, perl = TRUE)) > 0) {
    return("tensorflow-saved-model")
  }

  # no renderable content here; if there's at least one file, we can just serve
  # it as static content
  if (length(files) > 0) {
    return("static")
  }

  # there doesn't appear to be any content here we can use
  stop("No content to deploy; cannot detect content type.")
}

inferAppPrimaryDoc <- function(appPrimaryDoc, appFiles, appMode) {
  # If deploying an R Markdown, Quarto, or static content, infer a primary
  # document if one is not already specified.
  # Note: functionality in inferQuartoInfo() depends on primary doc inference
  # working the same across app modes.
  docAppModes <- c(
      "static",
      "rmd-shiny",
      "rmd-static",
      "quarto-shiny",
      "quarto-static"
  )
  if ((appMode %in% docAppModes) && is.null(appPrimaryDoc)) {
    # determine expected primary document extension
    ext <- ifelse(appMode == "static", "html?", "[Rq]md")

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
inferRPackageDependencies <- function(appMode, hasParameters, documentsHavePython, quartoInfo) {
  deps <- c()
  if (appMode == "rmd-static") {
    if (hasParameters) {
      # An Rmd with parameters needs shiny to run the customization app.
      deps <- c(deps, "shiny")
    }
    deps <- c(deps, "rmarkdown")
  }
  if (appMode == "quarto-static" && appUsesR(quartoInfo)) {
    # Quarto documents need R when the knitr execution engine is used, not always.
    deps <- c(deps, "rmarkdown")
  }
  if (appMode == "quarto-shiny") {
    # Quarto Shiny documents are executed with rmarkdown::run
    deps <- c(deps, "rmarkdown", "shiny")
  }
  if (appMode == "rmd-shiny") {
    deps <- c(deps, "rmarkdown", "shiny")
  }
  if (appMode == "shiny") {
    deps <- c(deps, "shiny")
  }
  if (appMode == "api") {
    deps <- c(deps, "plumber")
  }
  if (documentsHavePython && appUsesR(quartoInfo)) {
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
  conda <- file.path(miniconda, "bin", "conda")
  if (isWindows()) {
    conda <- paste(conda, ".exe", sep = "")
  }
  if (!file.exists(conda)) {
    stop(paste("Conda env prefix", prefix, "does not have the `conda` command line interface."))
  }
  conda
}

getPython <- function(path) {
  if (is.null(path)) {
    path <- Sys.getenv("RETICULATE_PYTHON",
                       unset = Sys.getenv("RETICULATE_PYTHON_FALLBACK"))
    if (path == "") {
      return(NULL)
    }
  }
  path.expand(path)
}

inferPythonEnv <- function(workdir, python, condaMode, forceGenerate) {
  # run the python introspection script
  env_py <- system.file("resources/environment.py", package = "rsconnect")
  args <- c(shQuote(env_py))
  if (condaMode || forceGenerate) {
    flags <- paste("-", ifelse(condaMode, "c", ""), ifelse(forceGenerate, "f", ""), sep = "")
    args <- c(args, flags)
  }
  args <- c(args, shQuote(workdir))

  tryCatch({
    # First check for reticulate. Then see if python is loaded in reticulate space, verify anaconda presence,
    # and verify that the user hasn't specified that they don't want their conda environment captured.
    if ("reticulate" %in% rownames(installed.packages()) && reticulate::py_available(initialize = FALSE) &&
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
                              forceGenerate, python = NULL, documentsHavePython = FALSE,
                              retainPackratDirectory = TRUE,
                              quartoInfo = NULL,
                              isCloud = FALSE,
                              image = NULL,
                              verbose = FALSE) {

  # provide package entries for all dependencies
  packages <- list()

  # potential problems with the bundled content.
  errorMessages <- NULL
  packageMessages <- NULL

  pyInfo   <- NULL

  # get package dependencies for non-static content deployment
  if (!identical(appMode, "static") &&
      !identical(appMode, "tensorflow-saved-model")) {

    # detect dependencies including inferred dependencies
    inferredRDependencies <- inferRPackageDependencies(
      appMode = appMode,
      hasParameters = hasParameters,
      documentsHavePython = documentsHavePython,
      quartoInfo = quartoInfo
    )

    # Skip snapshotting R dependencies if an app does not use R. Some
    # dependencies seem to be found based on the presence of Bioconductor
    # packages in the user's environment.
    if (appUsesR(quartoInfo)) {
      deps <- snapshotRDependencies(appDir, inferredRDependencies, verbose = verbose)
    } else {
      deps <- data.frame()
    }

    # construct package list from dependencies

    logger <- verboseLogger(verbose)
    for (i in seq_len(nrow(deps))) {
      name <- deps[i, "Package"]

      # get package info
      info <- as.list(deps[i, c("Source", "Repository")])

      # include github package info
      info <- c(info, as.list(deps[i, grep("Github", colnames(deps), perl = TRUE, value = TRUE)]))

      # get package description; note that we need to remove the
      # packageDescription S3 class from the object or jsonlite will refuse to
      # serialize it when building the manifest JSON
      # TODO: should we get description from packrat/desc folder?
      info$description <- suppressWarnings(unclass(utils::packageDescription(name)))

      # if description is NA, application dependency may not be installed
      if (is.na(info$description[1])) {
        errorMessages <- c(errorMessages, paste0(capitalize(assetTypeName), " depends on package \"",
                             name, "\" but it is not installed. Please resolve ",
                             "before continuing."))
        next
      }

      # validate package source (returns a message if there is a problem)
      packageMessages <- c(packageMessages, validatePackageSource(deps[i, ]))

      # good to go
      packages[[name]] <- info
    }
  }
  if (length(packageMessages)) {
    # Advice to help resolve installed packages that are not available using the
    # current set of configured repositories. Each package with a missing
    # repository has already been printed (see snapshotDependencizes).
    #
    # This situation used to trigger an error (halting deployment), but was
    # softened because:
    #   * CRAN-archived packages are not visible to our available.packages
    #     scanning.
    #   * Source-installed packages may be available after a manual server-side
    #     installation.
    #
    # That said, an incorrectly configured "repos" option is almost always the
    # cause.
    packageMessages <- c(packageMessages,
                         paste0(
                           "Unable to determine the source location for some packages. ",
                           "Packages should be installed from a package repository like ",
                           "CRAN or a version control system. Check that ",
                           "options('repos') refers to a package repository containing ",
                           "the needed package versions."
                         ))
    warning(paste(formatUL(packageMessages, "\n*"), collapse = "\n"), call. = FALSE, immediate. = TRUE)
  }

  needsPyInfo <- appUsesPython(quartoInfo) || "reticulate" %in% names(packages)
  if (needsPyInfo && !is.null(python)) {
    pyInfo <- inferPythonEnv(appDir, python, condaMode, forceGenerate)
    if (is.null(pyInfo$error)) {
      # write the package list into requirements.txt/environment.yml file in the bundle dir
      packageFile <- file.path(appDir, pyInfo$package_manager$package_file)
      cat(pyInfo$package_manager$contents, file = packageFile, sep = "\n")
      pyInfo$package_manager$contents <- NULL
    }
    else {
      errorMessages <- c(errorMessages, paste("Error detecting python environment:", pyInfo$error))
    }
  }

  if (length(errorMessages)) {
    stop(paste(formatUL(errorMessages, "\n*"), collapse = "\n"), call. = FALSE)
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
  if (!is.null(pyInfo)) {
    manifest$python <- pyInfo
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

validatePackageSource <- function(pkg) {
  if (isSCMSource(pkg$Source)) {
    return()
  }

  if (is.null(pkg$Repository) || is.na(pkg$Repository)) {
    return(sprintf("May be unable to deploy package dependency '%s'; could not determine a repository URL for the source '%s'.",
                   pkg$Package, pkg$Source))
  }

  return()
}

hasRequiredDevtools <- function() {
  "devtools" %in% .packages(all.available = TRUE) &&
    packageVersion("devtools") > "1.3"
}

preservePackageDescriptions <- function(bundleDir) {
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
    records <- utils::tail(read.dcf(lockFilePath), -1)
    lapply(seq_len(nrow(records)), function(i) {
      pkgName <- records[i, "Package"]
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
  totalSize <- 0
  totalFiles <- 0
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
      contentPaths <- file.path(f, contents)
      contentInfos <- file.info(contentPaths)

      totalSize <- totalSize + sum(contentInfos$size, na.rm = TRUE)
      totalFiles <- totalFiles + length(contentPaths)

      exploded <- c(exploded, contentPaths)
    } else {
      # not a directory; an ordinary file

      ourSize <- if (is.na(info$size)) { 0 } else { info$size }
      totalSize <- totalSize + ourSize
      totalFiles <- totalFiles + 1

      exploded <- c(exploded, f)
    }
    # Limits are being enforced after processing each entry on the
    # input. This means that an input directory needs to be fully
    # enumerated before issuing an error. This is different from the
    # approach by bundleFiles, which enforces limits while walking
    # directories.
    enforceBundleLimits(dir, totalSize, totalFiles)
  }
  exploded
}

# Run "quarto inspect" on the target and returns its output as a parsed object.
quartoInspect <- function(appDir = NULL, appPrimaryDoc = NULL, quarto = NULL) {
  if (is.null(quarto)) {
    return(NULL)
  }
  inspect <- NULL
  # If "quarto inspect appDir" fails, we will try "quarto inspect
  # appPrimaryDoc", so that we can support single files as well as projects.
  primaryDocPath <- file.path(appDir, appPrimaryDoc) # prior art: appHasParameters()
  for (path in c(appDir, primaryDocPath)) {
    args <- c("inspect", path.expand(path))
    tryCatch(
      {
        inspectOutput <- suppressWarnings(system2(quarto, args, stdout = TRUE, stderr = TRUE))
        inspect <- jsonlite::fromJSON(inspectOutput)
      },
      error = function(e) e
    )
    if (!is.null(inspect)) break
  }
  return(inspect)
}

# Attempt to gather Quarto version and engines, first from quarto inspect if a
# quarto executable is provided, and then from metadata.
inferQuartoInfo <- function(appDir, appPrimaryDoc, appFiles, quarto, metadata) {
  quartoInfo <- NULL
  if (!is.null(metadata$quarto_version)) {
    # Prefer metadata, because that means someone already ran quarto inspect
    quartoInfo <- list(
      "version" = metadata[["quarto_version"]],
      "engines" = metadata[["quarto_engines"]]
    )
  }
  if (is.null(quartoInfo) && !is.null(quarto)) {
    # If we don't yet have Quarto details, run quarto inspect ourselves

    # If no appPrimaryDoc has been provided, we will use the file that will be
    # chosen if this deployment ends up with an R Markdown or Quarto app mode.
    # This works because:

    # - App modes are only used to gate primary doc inference; the behavior does
    #   not differ between app modes.
    # - inferAppPrimaryDoc() returns appPrimaryDoc() if it is not null.
    tryCatch({
      appPrimaryDoc <- inferAppPrimaryDoc(
        appPrimaryDoc = appPrimaryDoc,
        appFiles = appFiles,
        appMode = "quarto-static"
      )
    }, error = function(e) {}
    )
    inspect <- quartoInspect(
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc,
      quarto = quarto
    )
    if (!is.null(inspect)) {
      quartoInfo <- list(
        version = inspect[["quarto"]][["version"]],
        engines = I(inspect[["engines"]])
      )
    }
  }
  return(quartoInfo)
}

appUsesR <- function(quartoInfo) {
  if (is.null(quartoInfo)) {
    # All non-Quarto content currently uses R by default.
    # To support non-R content in rsconnect, we could inspect appmode here.
    return(TRUE)
  }
  # R is used only supported with the "knitr" engine, not "jupyter" or "markdown"
  # Technically, "jupyter" content could support R.
  return("knitr" %in% quartoInfo[["engines"]])
}

appUsesPython <- function(quartoInfo) {
  if (is.null(quartoInfo)) {
    # No R-based, non-Quarto content uses Python by default.
    # Looking for Python chunks in Rmd needs to happen separately.
    return(FALSE)
  }
  # Python is a direct consequence of the "jupyter" engine; not "knitr" or "markdown".
  return("jupyter" %in% quartoInfo[["engines"]])
}


# Packrat Snapshots

# There are three functions here that do a lot of work here.
# snapshotRDependencies() calls addPackratSnapshot(), which calls
# performPackratSnapshot().

snapshotRDependencies <- function(appDir, implicit_dependencies = c(), verbose = FALSE) {

  # create a packrat "snapshot"

  addPackratSnapshot(appDir, implicit_dependencies, verbose = verbose)

  # TODO: should we care about lockfile version or packrat version?
  lockFilePath <- snapshotLockFile(appDir)
  df <- as.data.frame(read.dcf(lockFilePath), stringsAsFactors = FALSE)

  # get repos defined in the lockfile
  repos <- gsub("[\r\n]", " ", df[1, "Repos"])
  repos <- strsplit(unlist(strsplit(repos, "\\s*,\\s*", perl = TRUE)), "=", fixed = TRUE)
  repos <- setNames(
    sapply(repos, "[[", 2),
    sapply(repos, "[[", 1)
  )

  # read available.packages filters (allow user to override if necessary;
  # this is primarily to allow debugging)
  #
  # note that we explicitly exclude the "R_version" filter as we want to ensure
  # that packages which require newer versions of R than the one currently
  # in use can still be marked as available on CRAN -- for example, currently
  # the package "foreign" requires "R (>= 4.0.0)" but older versions of R
  # can still successfully install older versions from the CRAN archive
  filters <- getOption(
    "available_packages_filters",
    default = c("duplicates")
  )

  # get Bioconductor repos if any
  biocRepos <- repos[grep("BioC", names(repos), perl = TRUE, value = TRUE)]
  biocPackages <- if (length(biocRepos) > 0) {
    available.packages(
      contriburl = contrib.url(biocRepos, type = "source"),
      type = "source",
      filters = filters
    )
  }

  # read available packages
  repo.packages <- available.packages(
    contriburl = contrib.url(repos, type = "source"),
    type = "source",
    filters = filters
  )

  named.repos <- name.all.repos(repos)
  repo.lookup <- data.frame(
    name = names(named.repos),
    url = as.character(named.repos),
    contrib.url = contrib.url(named.repos, type = "source"),
    stringsAsFactors = FALSE)

  # get packages records defined in the lockfile
  records <- utils::tail(df, -1)

  # if the package is in a named CRAN-like repository capture it
  tmp <- lapply(seq_len(nrow(records)), function(i) {

    pkg <- records[i, "Package"]
    source <- records[i, "Source"]
    repository <- NA
    # capture Bioconcutor repository
    if (identical(source, "Bioconductor")) {
      if (pkg %in% biocPackages) {
        repository <- biocPackages[pkg, "Repository"]
      }
    } else if (isSCMSource(source)) {
      # leave source+SCM packages alone.
    } else if (pkg %in% rownames(repo.packages)) {
      # capture CRAN-like repository

      # Find this package in the set of available packages then use its
      # contrib.url to map back to the configured repositories.
      package.contrib <- repo.packages[pkg, "Repository"]
      package.repo.index <- vapply(repo.lookup$contrib.url,
                                   function(url) grepl(url, package.contrib, fixed = TRUE), logical(1))
      package.repo <- repo.lookup[package.repo.index, ][1, ]
      # If the incoming package comes from CRAN, keep the CRAN name in place
      # even if that means using a different name than the repos list.
      #
      # The "cran" source is a well-known location for shinyapps.io.
      #
      # shinyapps.io isn't going to use the manifest-provided CRAN URL,
      # but other consumers (Connect) will.
      if (tolower(source) != "cran") {
        source <- package.repo$name
      }
      repository <- package.repo$url
    }
    # validatePackageSource will emit a warning for packages with NA repository.
    data.frame(Source = source, Repository = repository)
  })
  records[, c("Source", "Repository")] <- do.call("rbind", tmp)
  return(records)
}

addPackratSnapshot <- function(bundleDir, implicit_dependencies = c(), verbose = FALSE) {

  logger <- verboseLogger(verbose)

  # if we discovered any extra dependencies, write them to a file for packrat to
  # discover when it creates the snapshot

  tempDependencyFile <- file.path(bundleDir, "__rsconnect_deps.R")
  if (length(implicit_dependencies) > 0) {
    extraPkgDeps <- paste0(lapply(implicit_dependencies,
                                  function(dep) {
                                    paste0("library(", dep, ")\n")
                                  }),
                           collapse = "")
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
  if (verbose) logger("Starting to perform packrat snapshot")
  tryCatch({
    performPackratSnapshot(bundleDir, verbose = verbose)
  }, error = function(e) {
    # if an error occurs while generating the snapshot, add a header to the
    # message for improved attribution
    e$msg <- paste0("----- Error snapshotting dependencies (Packrat) -----\n",
                    e$msg)

    # print a traceback if enabled
    if (isTRUE(getOption("rsconnect.error.trace"))) {
      traceback(x = sys.calls(), max.lines = 3)
    }

    # rethrow error so we still halt deployment
    stop(e)
  })
  if (verbose) logger("Completed performing packrat snapshot")

  # if we emitted a temporary dependency file for packrat's benefit, remove it
  # now so it isn't included in the bundle sent to the server
  if (file.exists(tempDependencyFile)) {
    unlink(tempDependencyFile)
  }

  preservePackageDescriptions(bundleDir)

  invisible()
}


performPackratSnapshot <- function(bundleDir, verbose = FALSE) {

  # move to the bundle directory
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(bundleDir)

  # ensure we snapshot recommended packages
  srp <- packrat::opts$snapshot.recommended.packages()
  packrat::opts$snapshot.recommended.packages(TRUE, persist = FALSE)
  on.exit(packrat::opts$snapshot.recommended.packages(srp, persist = FALSE),
          add = TRUE)

  # Force renv dependency scanning within packrat unless the option has been
  # explicitly configured. This is a no-op for older versions of packrat.
  renvDiscovery <- getOption("packrat.dependency.discovery.renv")
  if (is.null(renvDiscovery)) {
    oldDiscovery <- options("packrat.dependency.discovery.renv" = TRUE)
    on.exit(options(oldDiscovery), add = TRUE)
  }

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
                           verbose = verbose,
                           implicit.packrat.dependency = FALSE,
                           infer.dependencies = TRUE
                           )
  )

  # TRUE just to indicate success
  TRUE
}

snapshotLockFile <- function(appDir) {
  file.path(appDir, "packrat", "packrat.lock")
}

# Return TRUE when the source indicates that a package was installed from
# source or comes from a source control system. This indicates that we will
# not have a repostory URL; location is recorded elsewhere.
isSCMSource <- function(source) {
  tolower(source) %in% c("github", "gitlab", "bitbucket", "source")
}

# generate a random name prefixed with "repo_".
random.repo.name <- function() {
  paste("repo_", paste(sample(LETTERS, 8, replace = TRUE), collapse = ""), sep = "")
}

# Given a list of optionally named repository URLs, return a list of
# repository URLs where each element is named. Incoming names are preserved.
# Un-named repositories are given random names.
name.all.repos <- function(repos) {
  repo.names <- names(repos)
  if (is.null(repo.names)) {
    # names(X) return NULL when nothing is named. Build a same-sized vector of
    # empty-string names, which is the "no name here" placeholder value
    # produced when its input has a mix of named and un-named items.
    repo.names <- rep("", length(repos))
  }
  names(repos) <- sapply(repo.names, function(name) {
    if (name == "") {
      # Assumption: Random names are not repeated across a repo list.
      random.repo.name()
    } else {
      name
    }
  }, USE.NAMES = FALSE)
  repos
}

# get source packages from CRAN
availableCRANSourcePackages <- function() {
  available.packages("https://cran.rstudio.com/src/contrib", type = "source")
}
