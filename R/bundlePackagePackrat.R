snapshotPackratDependencies <- function(
  bundleDir,
  implicit_dependencies = character(),
  verbose = FALSE
) {
  addPackratSnapshot(bundleDir, implicit_dependencies, verbose = verbose)

  lockFilePath <- packratLockFile(bundleDir)
  df <- as.data.frame(read.dcf(lockFilePath), stringsAsFactors = FALSE)
  unlink(dirname(lockFilePath), recursive = TRUE)

  # get repos defined in the lockfile
  repos <- gsub("[\r\n]", " ", df[1, "Repos"])
  repos <- strsplit(
    unlist(strsplit(repos, "\\s*,\\s*", perl = TRUE)),
    "=",
    fixed = TRUE
  )
  repos <- setNames(
    sapply(repos, "[[", 2),
    sapply(repos, "[[", 1)
  )

  # get packages records defined in the lockfile
  records <- utils::tail(df, -1)
  if (nrow(records) == 0) {
    return(data.frame())
  }

  rownames(records) <- NULL
  records <- records[manifestPackageColumns(records)]
  records[c("Source", "Repository")] <- standardizeRecords(records, repos)
  records$description <- lapply(records$Package, package_record)

  records
}

standardizeRecords <- function(records, repos) {
  availablePackages <- availablePackages(repos)
  repos <- standardizeRepos(repos)

  rows <- lapply(seq_len(nrow(records)), function(i) {
    standardizePackratPackage(records[i, ], availablePackages, repos = repos)
  })
  rows <- lapply(rows, as.data.frame, stringsAsFactors = FALSE)
  rbind_fill(rows, c("Source", "Repository"))
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

standardizePackratPackage <- function(
  record,
  availablePackages,
  repos = character()
) {
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
  } else if (
    source == "CustomCRANLikeRepository" &&
      isDevVersion(record, availablePackages)
  ) {
    # Package was installed from source, but packrat guessed it was installed
    # from a known repo.
    repository <- NA_character_
    source <- NA_character_
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
  idx <- match(pkg, availablePackages[, "Package"])

  if (!is.na(idx)) {
    repo <- availablePackages[[idx, "Repository"]]
    # Strip `/src/contrib/*` from package repository: `contrib.url()`
    # adds /src/contrib, and RSPM adds additional directories
    gsub("/src/contrib.*$", "", repo)
  } else {
    NA_character_
  }
}

isDevVersion <- function(record, availablePackages) {
  idx <- match(record$Package, availablePackages[, "Package"])

  if (is.na(idx)) {
    return(FALSE)
  }

  local_version <- record$Version
  repo_version <- availablePackages[idx, "Version"]

  package_version(local_version) > package_version(repo_version)
}

addPackratSnapshot <- function(
  bundleDir,
  implicit_dependencies = character(),
  verbose = FALSE
) {
  # if we discovered any extra dependencies, write them to a file for packrat to
  # discover when it creates the snapshot
  recordExtraDependencies(bundleDir, implicit_dependencies)

  withCallingHandlers(
    performPackratSnapshot(bundleDir, verbose = verbose),
    error = function(err) {
      abort("Failed to snapshot dependencies", parent = err)
    },
    warning = function(cnd) {
      invokeRestart("muffleWarning")
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
  defer(packrat::opts$snapshot.recommended.packages(srp, persist = FALSE))

  # Force renv dependency scanning within packrat unless the option has been
  # explicitly configured. This is a no-op for older versions of packrat.
  renvDiscovery <- getOption("packrat.dependency.discovery.renv")
  if (is.null(renvDiscovery)) {
    old <- options("packrat.dependency.discovery.renv" = TRUE)
    defer(options(old))
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
