translateRenvToPackrat <- function(bundleDir) {
  renv <- jsonlite::read_json(renvLockFile(bundleDir))
  packrat <- packratFromRenv(renv)

  packratPath <- packratLockFile(bundleDir)
  dir.create(dirname(packratPath), showWarnings = FALSE)
  write.dcf(packrat, packratPath)
}

packratFromRenv <- function(renv) {
  meta <- packratMeta(renv)
  packages <- packratPackages(renv$Packages)
  rbind_fill(list(meta, packages))
}

packratMeta <- function(renv) {
  repos <- vapply(
    renv$R$Repositories,
    function(x) paste0(x$Name, "=", x$URL),
    character(1)
  )
  data.frame(
    PackratFormat = NA,
    PackratVersion = NA,
    RVersion = renv$R$Version,
    Repos = paste(repos, collapse = ",")
  )
}

packratPackages <- function(packages) {
  out <- lapply(packages, packratPackage)
  names(out) <- NULL
  out <- compact(out)
  out <- lapply(out, as.data.frame, stringsAsFactors = FALSE)
  rbind_fill(out)
}

packratPackage <- function(pkg) {
  # Don't include renv itself
  if (identical(pkg$Package, "renv")) {
    return(NULL)
  }

  if (identical(pkg$Repository, "CRAN")) {
    pkg$Source <- "CRAN"
  } else if (pkg$Source == "unknown") {
    pkg$Source <- "source"
  }

  if (identical(pkg$RemoteType, "standard")) {
    pkg <- pkg[!grepl("^Remote", names(pkg))]
  }

  # packrat hash != renv hash
  pkg$Hash <- NULL

  if (length(pkg$Requirements) > 0) {
    pkg$Requires <- paste0(unlist(pkg$Requirements), collapse = ", ")
  }
  pkg$Requirements <- NULL

  pkg
}

renvLockFile <- function(bundleDir) {
  file.path(bundleDir, "renv.lock")
}

showPackratTranslation <- function(path) {
  renv <- jsonlite::read_json(renvLockFile(path))
  packrat <- packratFromRenv(renv)
  showDcf(packrat)
}

showDcf <- function(df) {
  path <- tempfile()
  on.exit(path, add = TRUE)

  write.dcf(df, path)
  writeLines(readLines(path))
  invisible()
}
