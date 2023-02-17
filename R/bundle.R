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
  packages <- bundlePackages(
    appDir = appDir,
    appMode = appMode,
    assetTypeName = assetTypeName,
    hasParameters = hasParameters,
    documentsHavePython = documentsHavePython,
    quartoInfo = quartoInfo,
    verbose = verbose
  )

  pyInfo <- NULL
  needsPyInfo <- appUsesPython(quartoInfo) || "reticulate" %in% names(packages)
  if (needsPyInfo && !is.null(python)) {
    pyInfo <- inferPythonEnv(appDir, python, condaMode, forceGenerate)
    if (is.null(pyInfo$error)) {
      # write the package list into requirements.txt/environment.yml file in the bundle dir
      packageFile <- file.path(appDir, pyInfo$package_manager$package_file)
      cat(pyInfo$package_manager$contents, file = packageFile, sep = "\n")
      pyInfo$package_manager$contents <- NULL
    } else {
      stop(paste("Error detecting python environment:", pyInfo$error), call. = FALSE)
    }
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
    # emit dependencies to file
    extraPkgDeps <- paste0("library(", implicit_dependencies, ")\n")
    writeLines(extraPkgDeps, tempDependencyFile)

    # ensure temp file is cleaned up even if there's an error
    on.exit(unlink(tempDependencyFile), add = TRUE)
  }

  # generate the packrat snapshot
  logger("Starting to perform packrat snapshot")
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
  logger("Completed performing packrat snapshot")

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
    old <- options("packrat.dependency.discovery.renv" = TRUE)
    on.exit(options(old), add = TRUE)
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

standardizeAppFiles <- function(appDir, appFiles = NULL, appFileManifest = NULL) {
  # build the list of files to deploy -- implicitly (directory contents),
  # explicitly via list, or explicitly via manifest
  if (is.null(appFiles)) {
    if (is.null(appFileManifest)) {
      # no files supplied at all, just bundle the whole directory
      appFiles <- bundleFiles(appDir)
    } else {
      # manifest file provided, read it and apply
      check_file(appFileManifest)

      # read the filenames from the file
      manifestLines <- readLines(appFileManifest, warn = FALSE)

      # remove empty/comment lines and explode remaining
      manifestLines <- manifestLines[nzchar(manifestLines)]
      manifestLines <- manifestLines[!grepl("^#", manifestLines)]
      appFiles <- explodeFiles(appDir, manifestLines)
    }
  } else {
    # file list provided directly
    appFiles <- explodeFiles(appDir, appFiles)
  }
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
