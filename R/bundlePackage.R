bundlePackages <- function(appDir,
                           appMode,
                           assetTypeName,
                           hasParameters = FALSE,
                           documentsHavePython = FALSE,
                           quartoInfo = NULL,
                           verbose = FALSE
                           ) {
  if (appMode %in% c("static", "tensorflow-saved-model")) {
    return(list())
  }

  # Skip snapshotting R dependencies if an app does not use R. Some
  # dependencies seem to be found based on the presence of Bioconductor
  # packages in the user's environment.
  if (!appUsesR(quartoInfo)) {
    return(list())
  }

  # detect dependencies including inferred dependencies
  inferredRDependencies <- inferRPackageDependencies(
    appMode = appMode,
    hasParameters = hasParameters,
    documentsHavePython = documentsHavePython
  )
  deps <- snapshotRDependencies(appDir, inferredRDependencies, verbose = verbose)

  not_installed <- !vapply(deps$Package, is_installed, logical(1))
  if (any(not_installed)) {
    pkgs <- deps$Package[not_installed]
    cli::cli_abort(c(
      "All packages used by {assetTypeName} must be installed.",
      x = "Missing packages: {.pkg {pkgs}}."
    ))
  }

  missing_repo <- !isSCMSource(deps$Source) & is.na(deps$Repository)
  if (any(missing_repo)) {
    pkgs <- deps$Package[missing_repo]
    cli::cli_warn(c(
      "Packages require a known repository for install on remote system.",
      x = "Packages with unknown repository: {.pkg {pkgs}}.",
      i = paste0(
        "Automatic source detection relies on packages being installed from ",
        "a standard repository like CRAN or BioConductor, or from a version ",
        "control system like GitHub or GitLab."
      )
    ))
  }

  deps$description <- lapply(deps$Package, function(nm) {
    # Remove packageDescription S3 class so jsonlite can serialize
    # TODO: should we get description from packrat/desc folder?
    unclass(utils::packageDescription(nm))
  })

  github_cols <- grep("Github", colnames(deps), perl = TRUE, value = TRUE)
  packages <- deps[c("Source", "Repository", github_cols, "description")]
  packages_list <- lapply(seq_len(nrow(packages)), function(i) {
    as.list(packages[i, , drop = FALSE])
  })
  names(packages_list) <- deps$Package
  packages_list
}

## check for extra dependencies uses
inferRPackageDependencies <- function(appMode,
                                      hasParameters = FALSE,
                                      documentsHavePython = FALSE) {

  deps <- switch(appMode,
    "rmd-static" = c("rmarkdown", if (hasParameters) "shiny"),
    "quarto-static" = "rmarkdown",
    "quarto-shiny" = c("rmarkdown", "shiny"),
    "rmd-shiny" = c("rmarkdown", "shiny"),
    "shiny" = "shiny",
    "api" = "plumber"
  )
  if (documentsHavePython) {
    deps <- c(deps, "reticulate")
  }
  deps
}
