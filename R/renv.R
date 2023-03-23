parseRenvDependencies <- function(bundleDir, extraPackages = c()) {
  renv <- jsonlite::read_json(renvLockFile(bundleDir))

  repos <- setNames(
    vapply(renv$R$Repositories, "[[", "URL", FUN.VALUE = character(1)),
    vapply(renv$R$Repositories, "[[", "Name", FUN.VALUE = character(1))
  )

  packratPackages(renv$Packages)
}

packratPackages <- function(packages) {
  names(packages) <- NULL
  out <- lapply(packages, packratPackage)
  out <- compact(out)
  out <- lapply(out, as.data.frame, stringsAsFactors = FALSE)
  rbind_fill(out)
}

packratPackage <- function(pkg) {
  # Don't include renv itself
  if (identical(pkg$Package, "renv")) {
    return(NULL)
  }

  # Convert renv sources to packrat sources
  # https://github.com/rstudio/renv/blob/0.17.2/R/snapshot.R
  if (identical(pkg$Repository, "CRAN")) {
    pkg$Source <- "CRAN"
  } else if (pkg$Source == "unknown") {
    pkg$Source <- "source"
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
