snapshotRenvDependencies <- function(bundleDir,
                                     extraPackages = character(),
                                     verbose = FALSE) {
  recordExtraDependencies(bundleDir, extraPackages)

  old <- options(renv.verbose = FALSE, pkgType = "source")
  defer(options(old))

  renv::snapshot(
    bundleDir,
    type = "implicit",
    prompt = FALSE
  )
  defer({
    unlink(renvLockFile(bundleDir))
    unlink(file.path(bundleDir, "renv"), recursive = TRUE)
  })

  parseRenvDependencies(bundleDir)
}

parseRenvDependencies <- function(bundleDir) {
  renv <- jsonlite::read_json(renvLockFile(bundleDir))

  repos <- setNames(
    vapply(renv$R$Repositories, "[[", "URL", FUN.VALUE = character(1)),
    vapply(renv$R$Repositories, "[[", "Name", FUN.VALUE = character(1))
  )

  deps <- standardizeRenvPackages(renv$Packages, repos)
  deps
}

standardizeRenvPackages <- function(packages, repos) {
  repos <- standardizeRepos(repos)
  availablePackages <- availablePackages(repos)

  names(packages) <- NULL
  out <- lapply(
    packages,
    standardizeRenvPackage,
    availablePackages = availablePackages,
    repos = repos
  )
  out <- compact(out)
  out <- lapply(out, as.data.frame, stringsAsFactors = FALSE)
  rbind_fill(out)
}

standardizeRenvPackage <- function(pkg, availablePackages, repos = character()) {
  # Don't include renv itself
  if (identical(pkg$Package, "renv")) {
    return(NULL)
  }

  # Convert renv source to manifest source/repository
  # https://github.com/rstudio/renv/blob/0.17.2/R/snapshot.R#L730-L773

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
      pkg$Source <- findRepoName(pkg$Repository, repos)
      pkg$Repository <- findRepoUrl(pkg$Package, availablePackages)
    }
  } else if (pkg$Source == "Bioconductor") {
    pkg$Repository <- findRepoUrl(pkg$Package, availablePackages)
  } else if (pkg$Source == "unknown") {
    pkg$Source <- NA_character_
  } else if (pkg$Source %in% c("BitBucket", "GitHub", "GitLab")) {
    pkg$Source <- tolower(pkg$Source)
  }

  # Remove Remote fields that pak adds for "standard" installs from CRAN
  if (identical(pkg$RemoteType, "standard")) {
    pkg <- pkg[!grepl("^Remote", names(pkg))]
  }

  pkg[manifestPackageColumns(pkg)]
}

renvLockFile <- function(bundleDir) {
  file.path(bundleDir, "renv.lock")
}
