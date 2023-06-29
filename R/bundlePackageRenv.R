snapshotRenvDependencies <- function(bundleDir,
                                     extraPackages = character(),
                                     verbose = FALSE) {
  recordExtraDependencies(bundleDir, extraPackages)

  old <- options(
    renv.verbose = FALSE,
    renv.consent = TRUE
  )
  defer(options(old))

  renv::snapshot(bundleDir, prompt = FALSE)
  defer(removeRenv(bundleDir))

  parseRenvDependencies(bundleDir, snapshot = TRUE)
}

parseRenvDependencies <- function(bundleDir, snapshot = FALSE) {
  renv <- jsonlite::read_json(renvLockFile(bundleDir))

  repos <- setNames(
    vapply(renv$R$Repositories, "[[", "URL", FUN.VALUE = character(1)),
    vapply(renv$R$Repositories, "[[", "Name", FUN.VALUE = character(1))
  )
  deps <- standardizeRenvPackages(
    renv$Packages,
    repos,
    biocPackages = biocPackages(bundleDir)
  )
  if (nrow(deps) == 0) {
    return(data.frame())
  }

  if (snapshot) {
    # Can use system libraries
    deps$description <- lapply(deps$Package, package_record)
  } else {
    old <- options(renv.verbose = FALSE)
    defer(options(old))

    # Generate a library from the lockfile
    lib_dir <- dirCreate(file.path(bundleDir, "renv_library"))
    renv::restore(bundleDir, library = lib_dir, prompt = FALSE)
    defer(unlink(lib_dir, recursive = TRUE))

    deps$description <- lapply(deps$Package, package_record, lib_dir = lib_dir)
  }

  deps
}

standardizeRenvPackages <- function(packages, repos, biocPackages = NULL) {
  repos <- standardizeRepos(repos)
  availablePackages <- availablePackages(repos)

  names(packages) <- NULL
  out <- lapply(
    packages,
    standardizeRenvPackage,
    availablePackages = availablePackages,
    biocPackages = biocPackages,
    repos = repos
  )
  out <- compact(out)
  out <- lapply(out, as.data.frame, stringsAsFactors = FALSE)
  rbind_fill(out)
}

standardizeRenvPackage <- function(pkg,
                                   availablePackages,
                                   biocPackages = NULL,
                                   repos = character(),
                                   bioc) {
  # Don't include renv itself
  if (identical(pkg$Package, "renv")) {
    return(NULL)
  }

  # Convert renv source to manifest source/repository
  # https://github.com/rstudio/renv/blob/0.17.2/R/snapshot.R#L730-L773

  if (is.null(pkg$Repository) && !is.null(pkg$RemoteRepos) && grepl("bioconductor.org", pkg$RemoteRepos)) {
    # Work around bug where renv fails to detect BioC package installed by pak
    # https://github.com/rstudio/renv/issues/1202
    pkg$Source <- "Bioconductor"
  }

  if (pkg$Source == "Repository") {
    if (pkg$Repository == "CRAN") {
      if (isDevVersion(pkg, availablePackages)) {
        pkg$Source <- NA_character_
        pkg$Repository <- NA_character_
      } else {
        pkg$Source <- "CRAN"
        pkg$Repository <- findRepoUrl(pkg$Package, availablePackages)
      }
    } else {
      # $Repository comes from DESCRIPTION and is set by repo, so can be
      # anything. So we must look up from the package name
      pkg$Repository <- findRepoUrl(pkg$Package, availablePackages)
      pkg$Source <- findRepoName(pkg$Repository, repos)
    }
  } else if (pkg$Source == "Bioconductor") {
    pkg$Repository <- findRepoUrl(pkg$Package, availablePackages)
    if (is.na(pkg$Repository)) {
      # Try packages defined from default bioC repos
      pkg$Repository <- findRepoUrl(pkg$Package, biocPackages)
    }
  } else if (pkg$Source == "unknown") {
    pkg$Source <- NA_character_
  } else if (pkg$Source %in% c("Bitbucket", "GitHub", "GitLab")) {
    pkg$Source <- tolower(pkg$Source)
  }

  # Remove Remote fields that pak adds for "standard" installs from CRAN
  if (identical(pkg$RemoteType, "standard")) {
    pkg <- pkg[!grepl("^Remote", names(pkg))]
  }

  pkg[manifestPackageColumns(pkg)]
}

biocPackages <- function(bundleDir) {
  signal("evaluating", class = "rsconnect_biocRepos") # used for testing
  availablePackages(biocRepos(bundleDir))
}
biocRepos <- function(bundleDir) {
  repos <- getFromNamespace("renv_bioconductor_repos", "renv")(bundleDir)
  repos[setdiff(names(repos), "CRAN")]
}

renvLockFile <- function(bundleDir) {
  file.path(bundleDir, "renv.lock")
}

removeRenv <- function(path) {
  unlink(renvLockFile(path))
  unlink(file.path(path, "renv"), recursive = TRUE)
}
