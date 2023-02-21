bundlePackages <- function(appDir,
                           appMode,
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

  packageMessages <- c()
  errorMessages <- c()
  packages <- list()

  # construct package list from dependencies
  for (i in seq_len(nrow(deps))) {
    name <- deps[i, "Package"]

    # get package info
    info <- as.list(deps[i, c("Source", "Repository")])

    # include github package info
    info <- c(info, as.list(deps[i, grep("Github", colnames(deps), perl = TRUE, value = TRUE)]))

    # get package description; note that we need to remove the
    # packageDescription S3 class from the object or jsonlite will refuse to
    # serialize it when building the manifest JSON
    # TODO: should we get description from packrat/desc folder?
    info$description <- suppressWarnings(unclass(utils::packageDescription(name)))

    # TODO(HW): should this code be removed? I don't see how it is ever reached
    # if description is NA, application dependency may not be installed
    if (is.na(info$description[1])) {
      errorMessages <- c(
        errorMessages,
        paste0(
          "Deployment depends on package \"", name, "\"",
          "but it is not installed. Please resolve before continuing."
        )
      )
      next
    }

    # validate package source (returns a message if there is a problem)
    packageMessages <- c(packageMessages, validatePackageSource(deps[i, ]))

    # good to go
    packages[[name]] <- info
  }

  if (length(packageMessages)) {
    # Advice to help resolve installed packages that are not available using the
    # current set of configured repositories. Each package with a missing
    # repository has already been printed (see snapshotDependencizes).
    #
    # This situation used to trigger an error (halting deployment), but was
    # softened because:
    #   * CRAN-archived packages are not visible to our available.packages
    #     scanning.
    #   * Source-installed packages may be available after a manual server-side
    #     installation.
    #
    # That said, an incorrectly configured "repos" option is almost always the
    # cause.
    packageMessages <- c(
      packageMessages,
      paste0(
        "Unable to determine the source location for some packages. ",
        "Packages should be installed from a package repository like ",
        "CRAN or a version control system. Check that ",
        "options('repos') refers to a package repository containing ",
        "the needed package versions."
      )
    )
    warning(bullets(packageMessages), call. = FALSE, immediate. = TRUE)
  }

  if (length(errorMessages)) {
    stop(bullets(errorMessages), call. = FALSE)
  }

  packages
}

bullets <- function(x) {
  bullets <- lapply(x, function(x) {
    paste0(strwrap(x, initial = "* ", exdent = 2, width = 72), "\n", collapse = "")
  })
  paste0(bullets, "\n", collapse = "")
}

## check for extra dependencies uses
inferRPackageDependencies <- function(appMode,
                                      hasParameters = FALSE,
                                      documentsHavePython = FALSE) {

  deps <- c()
  if (appMode == "rmd-static") {
    if (hasParameters) {
      # An Rmd with parameters needs shiny to run the customization app.
      deps <- c(deps, "shiny")
    }
    deps <- c(deps, "rmarkdown")
  }
  if (appMode == "quarto-static") {
    # Quarto documents need R when the knitr execution engine is used, not always.
    deps <- c(deps, "rmarkdown")
  }
  if (appMode == "quarto-shiny") {
    # Quarto Shiny documents are executed with rmarkdown::run
    deps <- c(deps, "rmarkdown", "shiny")
  }
  if (appMode == "rmd-shiny") {
    deps <- c(deps, "rmarkdown", "shiny")
  }
  if (appMode == "shiny") {
    deps <- c(deps, "shiny")
  }
  if (appMode == "api") {
    deps <- c(deps, "plumber")
  }
  if (documentsHavePython) {
    deps <- c(deps, "reticulate")
  }
  unique(deps)
}

validatePackageSource <- function(pkg) {
  if (isSCMSource(pkg$Source)) {
    return()
  }

  if (is.null(pkg$Repository) || is.na(pkg$Repository)) {
    return(sprintf(
      "May be unable to deploy package dependency '%s'; could not determine a repository URL for the source '%s'.",
      pkg$Package,
      pkg$Source
    ))
  }

  return()
}

appUsesR <- function(quartoInfo) {
  if (is.null(quartoInfo)) {
    # All non-Quarto content currently uses R by default.
    # To support non-R content in rsconnect, we could inspect appmode here.
    return(TRUE)
  }
  # R is used only supported with the "knitr" engine, not "jupyter" or "markdown"
  # Technically, "jupyter" content could support R.
  return("knitr" %in% quartoInfo[["engines"]])
}
