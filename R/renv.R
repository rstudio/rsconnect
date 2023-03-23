snapshotRenvDependencies <- function(bundleDir, extraPackages, verbose = FALSE) {
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

  deps <- packratPackages(renv$Packages, repos)
  deps
}

packratPackages <- function(packages, repos) {
  availablePackages <- availablePackages(repos)

  names(packages) <- NULL
  out <- lapply(
    packages,
    packratPackage,
    repos = repos,
    availablePackages = availablePackages
  )
  out <- compact(out)
  out <- lapply(out, as.data.frame, stringsAsFactors = FALSE)
  rbind_fill(out)
}

packratPackage <- function(pkg, repos, availablePackages) {
  # Don't include renv itself
  if (identical(pkg$Package, "renv")) {
    return(NULL)
  }

  # Convert renv source to manifest source/repository
  # https://github.com/rstudio/renv/blob/0.17.2/R/snapshot.R#L730-L773
  #
  # Unlike standardizePackageSource() we don't worry about local installs
  # of dev versions of CRAN packages, because these are much less likely to
  # end up inside of renv lockfiles
  # if (pkg$Package == "BiocGenerics") browser()

  if (pkg$Source == "Repository") {
    pkg$Repository <- findRepoUrl(pkg$Package, availablePackages)
    pkg$Source <- findRepoName(pkg$Repository, repos)
  } else if (pkg$Source == "Bioconductor") {
    pkg$Repository <- findRepoUrl(pkg$Package, availablePackages)
  } else if (pkg$Source == "unknown") {
    pkg$Source <- NA
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

showDcf <- function(df) {
  write.dcf(df, stdout())
  invisible()
}
