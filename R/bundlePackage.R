bundlePackages <- function(appDir,
                           appMode,
                           hasParameters = FALSE,
                           documentsHavePython = FALSE,
                           quartoInfo = NULL,
                           verbose = FALSE,
                           error_call = caller_env()
                           ) {
  if (appMode %in% c("static", "tensorflow-saved-model")) {
    return(list())
  }

  # Skip snapshotting R dependencies if an app does not use R. Some
  # dependencies seem to be found based on the presence of Bioconductor
  # packages in the user's environment.
  if (!appUsesR(quartoInfo)) {
    return(list())
  }

  # detect dependencies including inferred dependencies
  inferredRDependencies <- inferRPackageDependencies(
    appMode = appMode,
    hasParameters = hasParameters,
    documentsHavePython = documentsHavePython
  )
  deps <- snapshotRDependencies(appDir, inferredRDependencies, verbose = verbose)
  checkBundlePackages(deps, call = error_call)

  deps$description <- lapply(deps$Package, function(nm) {
    # Remove packageDescription S3 class so jsonlite can serialize
    # TODO: should we get description from packrat/desc folder?
    unclass(utils::packageDescription(nm))
  })

  # Connect prefers that packrat/packrat.lock file, but will use the manifest
  # if needed. shinyapps.io only uses the manifest, and only supports Github
  # remotes, not Bitbucket or Gitlab.
  github_cols <- grep("Github", colnames(deps), perl = TRUE, value = TRUE)
  packages <- deps[c("Source", "Repository", github_cols, "description")]
  packages_list <- lapply(seq_len(nrow(packages)), function(i) {
    as.list(packages[i, , drop = FALSE])
  })
  names(packages_list) <- deps$Package
  packages_list
}

checkBundlePackages <- function(deps, call = caller_env()) {
  not_installed <- !vapply(deps$Package, is_installed, logical(1))
  if (any(not_installed)) {
    pkgs <- deps$Package[not_installed]
    cli::cli_abort(
      c(
        "All packages used by the asset must be installed.",
        x = "Missing packages: {.pkg {pkgs}}."
      ),
      call = call
    )
  }

  unknown_source <- is.na(deps$Source)
  if (any(unknown_source)) {
    pkgs <- deps$Package[unknown_source]
    cli::cli_warn(
      c(
        "Local packages must be installed from a supported source.",
        x = "Unsupported packages: {.pkg {pkgs}}.",
        i = "Supported sources are CRAN and CRAN-like repositories, BioConductor, GitHub, GitLab, and Bitbucket.",
        i = "See {.fun rsconnect::appDependencies} for more details."
      ),
      call = call
    )
  }
}

## check for extra dependencies uses
inferRPackageDependencies <- function(appMode,
                                      hasParameters = FALSE,
                                      documentsHavePython = FALSE) {
  deps <- switch(appMode,
    "rmd-static" = c("rmarkdown", if (hasParameters) "shiny"),
    "quarto-static" = "rmarkdown",
    "quarto-shiny" = c("rmarkdown", "shiny"),
    "rmd-shiny" = c("rmarkdown", "shiny"),
    "shiny" = "shiny",
    "api" = "plumber"
  )
  if (documentsHavePython) {
    deps <- c(deps, "reticulate")
  }
  deps
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
