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
                              appPrimaryDoc, users, condaMode,
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

  # get packages records defined in the lockfile
  records <- utils::tail(df, -1)
  records[c("Source", "Repository")] <- findPackageRepoAndSource(records, repos)
  records
}

findPackageRepoAndSource <- function(records, repos) {
  # read available.packages filters (allow user to override if necessary;
  # this is primarily to allow debugging)
  #
  # note that we explicitly exclude the "R_version" filter as we want to ensure
  # that packages which require newer versions of R than the one currently
  # in use can still be marked as available on CRAN -- for example, currently
  # the package "foreign" requires "R (>= 4.0.0)" but older versions of R
  # can still successfully install older versions from the CRAN archive
  filters <- getOption("available_packages_filters", default = "duplicates")

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
    stringsAsFactors = FALSE
  )

  # Sources are created by packrat:
  # https://github.com/rstudio/packrat/blob/v0.9.0/R/pkg.R#L328
  tmp <- lapply(seq_len(nrow(records)), function(i) {
    pkg <- records[i, "Package"]
    source <- records[i, "Source"]
    repository <- NA
    if (identical(source, "Bioconductor") && pkg %in% biocPackages) {
      # installed from known Bioconductor repo
      repository <- biocPackages[pkg, "Repository"]
    } else if (source %in% c("github", "bitbucket", "gitlab")) {
      # leave SCM packages alone.
    } else if (source %in% c("CRAN", "CustomCRANLikeRepository")) {
      if (!pkg %in% rownames(repo.packages)) {
        # No record in available.packages(); so leave source/repository
        # alone in the hope that its an archived package (#508)
      } else {
        repo_version <- package_version(repo.packages[pkg, "Version"])
        local_version <- package_version(records[i, "Version"])

        # If the name of a source-installed package matches a CRAN package,
        # packrat automatically fills in the repository information. But that
        # causes problems if you're working with a development version that's
        # newer than CRAN.
        if (local_version > repo_version) {
          source <- NA
        } else {
          # Find this package in the set of available packages then use its
          # contrib.url to map back to the configured repositories.
          package.contrib <- repo.packages[pkg, "Repository"]
          package.repo.index <- grepl(
            repo.lookup$contrib.url,
            package.contrib,
            fixed = TRUE
          )
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
      }
    } else {
      # Unsupported source, like "source" (i.e. local install that doens't
      # match name of CRAN package)
      source <- NA
    }
    # validatePackageSource will emit a warning for packages with NA repository.
    data.frame(Source = source, Repository = repository, stringsAsFactors = FALSE)
  })
  do.call("rbind", tmp)
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


appUsesPython <- function(quartoInfo) {
  if (is.null(quartoInfo)) {
    # No R-based, non-Quarto content uses Python by default.
    # Looking for Python chunks in Rmd needs to happen separately.
    return(FALSE)
  }
  # Python is a direct consequence of the "jupyter" engine; not "knitr" or "markdown".
  return("jupyter" %in% quartoInfo[["engines"]])
}
