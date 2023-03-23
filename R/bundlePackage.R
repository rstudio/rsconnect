bundlePackages <- function(bundleDir,
                           appMode,
                           extraPackages = character(),
                           quiet = FALSE,
                           verbose = FALSE,
                           error_call = caller_env()) {

  deps <- computePackageDependencies(
    bundleDir,
    extraPackages,
    quiet = quiet,
    verbose = verbose
  )
  if (nrow(deps) == 0) {
    return(list())
  }
  checkBundlePackages(deps, call = error_call)

  # TODO: figure out how to get from renv library, if used
  copyPackageDescriptions(bundleDir, deps$Package)
  deps$description <- lapply(deps$Package, function(nm) {
    # Remove packageDescription S3 class so jsonlite can serialize
    unclass(utils::packageDescription(nm))
  })

  packages_list <- lapply(seq_len(nrow(deps)), function(i) {
    out <- as.list(deps[i, , drop = FALSE])
    out$description <- out$description[[1]]
    out
  })
  names(packages_list) <- deps$Package
  packages_list
}

computePackageDependencies <- function(bundleDir,
                                       extraPackages = character(),
                                       quiet = FALSE,
                                       verbose = FALSE) {
  if (file.exists(renvLockFile(bundleDir))) {
    # This ignores extraPackages; if you're using a lockfile it's your
    # responsibility to install any other packages you need
    taskStart(quiet, "Capturing R dependencies from renv.lockfile")
    deps <- parseRenvDependencies(bundleDir)
    unlink(renvLockFile(bundleDir))
  } else if (isFALSE(getOption("rsconnect.packrat", FALSE))) {
    taskStart(quiet, "Capturing R dependencies with renv")
    # TODO: give user option to choose between implicit and explicit
    deps <- snapshotRenvDependencies(bundleDir, extraPackages, verbose = verbose)
  } else {
    taskStart(quiet, "Capturing R dependencies with packrat")
    deps <- snapshotPackratDependencies(bundleDir, extraPackages, verbose = verbose)
  }
  taskComplete(quiet, "Found {nrow(deps)} dependenc{?y/ies}.")

  deps
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
    cli::cli_abort(
      c(
        "All packages must be installed from a reproducible location.",
        x = "Can't re-install packages installed from source: {.pkg {pkgs}}.",
        i = "See {.fun rsconnect::appDependencies} for more details."
      ),
      call = call
    )
  }
}

# Copy all the DESCRIPTION files we're relying on into packrat/desc.
# That directory will contain one file for each package, e.g.
# packrat/desc/shiny will be the shiny package's DESCRIPTION.
copyPackageDescriptions <- function(bundleDir, packages) {
  descDir <- file.path(bundleDir, "packrat", "desc")
  dir.create(descDir, showWarnings = FALSE, recursive = TRUE)

  descPaths <- file.path(find.package(packages), "DESCRIPTION")
  file.copy(descPaths, file.path(descDir, packages))
  invisible()
}

snapshotPackratDependencies <- function(bundleDir,
                                        implicit_dependencies = character(),
                                        verbose = FALSE) {

  addPackratSnapshot(bundleDir, implicit_dependencies, verbose = verbose)

  lockFilePath <- packratLockFile(bundleDir)
  df <- as.data.frame(read.dcf(lockFilePath), stringsAsFactors = FALSE)
  unlink(dirname(lockFilePath), recursive = TRUE)

  # get repos defined in the lockfile
  repos <- gsub("[\r\n]", " ", df[1, "Repos"])
  repos <- strsplit(unlist(strsplit(repos, "\\s*,\\s*", perl = TRUE)), "=", fixed = TRUE)
  repos <- setNames(
    sapply(repos, "[[", 2),
    sapply(repos, "[[", 1)
  )

  # get packages records defined in the lockfile
  records <- utils::tail(df, -1)
  rownames(records) <- NULL
  records <- records[manifestPackageColumns(records)]
  records[c("Source", "Repository")] <- standardizeRecords(records, repos)
  records
}

manifestPackageColumns <- function(df) {
  github_cols <- grep("^(Github|Remote)", names(df), perl = TRUE, value = TRUE)
  intersect(names(df), c("Package", "Version", "Source", "Repository", github_cols))
}

standardizeRecords <- function(records, repos) {
  availablePackages <- availablePackages(repos)
  repos <- standardizeRepos(repos)

  rows <- lapply(seq_len(nrow(records)), function(i) {
    standardizePackageSource(records[i, ], availablePackages, repos = repos)
  })
  rows <- lapply(rows, as.data.frame, stringsAsFactors = FALSE)
  rbind_fill(rows, c("Source", "Repository"))
}

availablePackages <- function(repos) {
  # read available.packages filters (allow user to override if necessary;
  # this is primarily to allow debugging)
  #
  # note that we explicitly exclude the "R_version" filter as we want to ensure
  # that packages which require newer versions of R than the one currently
  # in use can still be marked as available on CRAN -- for example, currently
  # the package "foreign" requires "R (>= 4.0.0)" but older versions of R
  # can still successfully install older versions from the CRAN archive
  available.packages(
    repos = repos,
    type = "source",
    filters = getOption("available_packages_filters", default = "duplicates")
  )
}

standardizeRepos <- function(repos) {
  # Ensure that each repository has a unique name
  names(repos) <- ifelse(
    names2(repos) == "",
    paste0("repo_", seq_along(repos)),
    names2(repos)
  )

  # And strip trailing /
  repos <- gsub("/$", "", repos)

  repos
}

standardizePackageSource <- function(record, availablePackages, repos = character()) {
  pkg <- record$Package
  source <- record$Source

  # source types are defined by packrat:
  # https://github.com/rstudio/packrat/blob/v0.9.0/R/pkg.R#L328
  if (source %in% c("github", "gitlab", "bitbucket")) {
    # SCM information is recorded elsewhere
    repository <- NA_character_
  } else if (source == "source") {
    # can't install source packages elsewhere
    repository <- NA_character_
    source <- NA_character_
  } else if (source == "CustomCRANLikeRepository") {
    # Package was installed from source, but packrat guessed it was installed
    # from a known repo. We need to check that it's not actually a dev version
    if (isDevVersion(record, availablePackages)) {
      repository <- NA_character_
      source <- NA_character_
    } else {
      repository <- findRepoUrl(pkg, availablePackages)
      source <- findRepoName(repository, repos)
    }
  } else if (source %in% c("CRAN", "Bioconductor")) {
    # shinyapps & posit.cloud will ignore, but connect will use (unless admin
    # has set up an override)
    repository <- findRepoUrl(pkg, availablePackages)
  } else {
    # Installed from custom repository. Find URL from available.packages()
    # and then name from repos.
    repository <- findRepoUrl(pkg, availablePackages)
    source <- findRepoName(repository, repos)
  }
  list(Source = source, Repository = repository)
}

findRepoName <- function(repository, repos) {
  idx <- match(repository, repos)
  names(repos)[idx]
}

findRepoUrl <- function(pkg, availablePackages) {
  if (pkg %in% rownames(availablePackages)) {
    repo <- availablePackages[pkg, "Repository"]
    # Strip `/src/contrib/*` from package repository: `contrib.url()`
    # adds /src/contrib, and RSPM adds additional directories
    gsub("/src/contrib.*$", "", repo)
  } else {
    NA_character_
  }
}

isDevVersion <- function(record, availablePackages) {
  if (!record$Package %in% rownames(availablePackages)) {
    return(FALSE)
  }

  local_version <- record$Version
  repo_version <- availablePackages[record$Package, "Version"]

  package_version(local_version) > package_version(repo_version)
}

addPackratSnapshot <- function(bundleDir,
                               implicit_dependencies = character(),
                               verbose = FALSE) {
  # if we discovered any extra dependencies, write them to a file for packrat to
  # discover when it creates the snapshot
  recordExtraDependencies(bundleDir, implicit_dependencies)

  withCallingHandlers(
    performPackratSnapshot(bundleDir, verbose = verbose),
    error = function(err) {
      abort("Failed to snapshot dependencies", parent = err)
    }
  )

  invisible()
}

recordExtraDependencies <- function(bundleDir, pkgs, env = caller_env()) {
  if (length(pkgs) == 0) {
    return()
  }

  depPath <- file.path(bundleDir, "__rsconnect_deps.R")
  writeLines(paste0("library(", pkgs, ")\n"), depPath)

  # Automatically delete when the _caller_ finishes
  defer(unlink(depPath), env = env)
  invisible()
}


performPackratSnapshot <- function(bundleDir, verbose = FALSE) {
  # ensure we snapshot recommended packages
  srp <- packrat::opts$snapshot.recommended.packages()
  packrat::opts$snapshot.recommended.packages(TRUE, persist = FALSE)
  on.exit(
    packrat::opts$snapshot.recommended.packages(srp, persist = FALSE),
    add = TRUE
  )

  # Force renv dependency scanning within packrat unless the option has been
  # explicitly configured. This is a no-op for older versions of packrat.
  renvDiscovery <- getOption("packrat.dependency.discovery.renv")
  if (is.null(renvDiscovery)) {
    old <- options("packrat.dependency.discovery.renv" = TRUE)
    on.exit(options(old), add = TRUE)
  }

  # attempt to eagerly load the BiocInstaller or BiocManaager package if
  # installed, to work around an issue where attempts to load the package could
  # fail within a 'suppressMessages()' context
  packages <- c("BiocManager", "BiocInstaller")
  for (package in packages) {
    if (is_installed(package)) {
      requireNamespace(package, quietly = TRUE)
      break
    }
  }

  suppressMessages(
    packrat::.snapshotImpl(
      project = bundleDir,
      snapshot.sources = FALSE,
      fallback.ok = TRUE,
      verbose = verbose,
      implicit.packrat.dependency = FALSE
    )
  )

  invisible()
}

packratLockFile <- function(bundleDir) {
  file.path(bundleDir, "packrat", "packrat.lock")
}
