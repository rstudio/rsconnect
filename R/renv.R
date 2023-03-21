translateRenvToPackrat <- function(bundleDir) {
  renv <- jsonlite::read_json(renvLockFile(bundleDir))

  meta <- packratMeta(renv)
  packages <- packratPackages(renv$Packages)
  packrat <- rbind_fill(list(meta, packages))

  browser()
  packratPath <- packratLockFile(bundleDir)
  dir.create(dirname(packratPath), showWarnings = FALSE)
  write.dcf(packrat, packratPath)
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
  out <- lapply(out, as.data.frame, stringsAsFactors = FALSE)
  rbind_fill(out)
}

packratPackage <- function(pkg) {
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
