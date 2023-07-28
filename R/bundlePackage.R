bundlePackages <- function(bundleDir,
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

  # Manifest packages used to generate packrat file on Connect
  # https://github.com/rstudio/connect/blob/v2023.03.0/src/connect/manifest/convert.go#L261-L320
  packages_list <- lapply(seq_len(nrow(deps)), function(i) {
    out <- as.list(deps[i, , drop = FALSE])
    out$description <- out$description[[1]]
    out$Package <- NULL
    out$Version <- NULL
    out
  })
  names(packages_list) <- deps$Package

  packages_list
}

usePackrat <- function() {
  # Use RSCONNECT_PACKRAT when it has any value; fall-back to rsconnect.packrat when the environment
  # variable is unset.
  value <- Sys.getenv("RSCONNECT_PACKRAT", unset = NA)
  if (is.na(value)) {
    value <- getOption("rsconnect.packrat", default = FALSE)
  }

  return(truthy(value))
}

computePackageDependencies <- function(bundleDir,
                                       extraPackages = character(),
                                       quiet = FALSE,
                                       verbose = FALSE) {

  if (usePackrat()) {
    taskStart(quiet, "Capturing R dependencies with packrat")
    # Remove renv.lock so the packrat call to renv::dependencies does not report an implicit renv
    # dependency. Mirrors rsconnect before 1.0.0, which did not include renv.lock in bundles.
    # https://github.com/rstudio/rsconnect/blob/v0.8.29/R/bundle.R#L96
    removeRenv(bundleDir)
    deps <- snapshotPackratDependencies(bundleDir, extraPackages, verbose = verbose)
  } else if (file.exists(renvLockFile(bundleDir))) {
    # This ignores extraPackages; if you're using a lockfile it's your
    # responsibility to install any other packages you need
    taskStart(quiet, "Capturing R dependencies from renv.lock")
    deps <- parseRenvDependencies(bundleDir)
    # Once we've captured the deps, we can remove the renv directory
    # from the bundle (retaining the renv.lock).
    removeRenv(bundleDir, lockfile = FALSE)
  } else {
    taskStart(quiet, "Capturing R dependencies with renv")
    # TODO: give user option to choose between implicit and explicit
    deps <- snapshotRenvDependencies(bundleDir, extraPackages, verbose = verbose)
  }
  taskComplete(quiet, "Found {nrow(deps)} dependenc{?y/ies}")

  deps
}

checkBundlePackages <- function(deps, call = caller_env()) {
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

manifestPackageColumns <- function(df) {
  # Fields defined in https://bit.ly/42CbD4P
  # Most fields are retrieved from the complete embedded description.
  # shinyapps.io needs GitHub fields for backward compatibility

  github_cols <- grep("^Github", names(df), perl = TRUE, value = TRUE)
  intersect(
    c("Package", "Version", "Source", "Repository", github_cols),
    names(df)
  )
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

package_record <- function(name, lib_dir = NULL) {
  path <- system.file("DESCRIPTION", package = name, lib.loc = lib_dir)
  as.list(as.data.frame(read.dcf(path)))
}
