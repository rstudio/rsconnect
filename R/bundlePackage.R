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

  # TODO: figure out how to get from renv library, if used
  copyPackageDescriptions(bundleDir, deps$Package)
  deps$description <- lapply(deps$Package, function(nm) {
    # Remove packageDescription S3 class so jsonlite can serialize
    unclass(utils::packageDescription(nm))
  })

  # Manifest packages used to generate packrat file on Connect
  # https://github.com/rstudio/connect/blob/v2023.03.0/src/connect/manifest/convert.go#L261-L320
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

manifestPackageColumns <- function(df) {
  # Fields defined in https://github.com/rstudio/lucid-legacy-builder/blob/v2023.02.16-1/src/lucid_legacy_builder/resources/schema/manifest/versions/1/manifest.json#L45-L91
  # Also include Remote* fields for forward compatibility

  github_cols <- grep("^(Github|Remote)", names(df), perl = TRUE, value = TRUE)
  intersect(names(df), c("Package", "Version", "Source", "Repository", github_cols))
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
