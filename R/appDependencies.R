#' Detect application dependencies
#'
#' @description
#' `appDependencies()` recursively detects all R package dependencies for an
#' application by parsing all `.R` and `.Rmd` files and looking for calls
#' to `library()`, `require()`, `requireNamespace()`, `::`, and so on.
#' It then adds implicit dependencies (i.e. an `.Rmd` requires Rmarkdown)
#' and adds all recursive dependencies to create a complete manifest of
#' package packages need to be installed to run the app.
#'
#' # Remote installation
#'
#' When deployed, the app must first install all of these packages, and
#' rsconnect does its best to ensure that all the packages have the same
#' version as you are running locally. It knows how to install packages from
#' the following sources:
#'
#' * CRAN (`CRAN`) and CRAN-like repositories (`CustomCRANLikeRepository`).
#' * BioConductor (`Bioconductor`)
#' * Packages installed from GitHub (`github`), GitLab (`gitlab`), or
#'   BitBucket (`bitbucket`).
#'
#' It does not know how to install packages that you have built and installed
#' locally so if you attempt to deploy an app that depends on such a package
#' it will fail. To resolve this issue, you'll need to install from a known
#' source.
#'
#' # Suggested packages
#'
#' The `Suggests` field is not included when determining recursive dependencies,
#' so it's possible that not every package required to run your application will
#' be detected.
#'
#' For example, ggplot2's `geom_hex()` requires the hexbin package to be
#' installed, but it is only suggested by ggplot2. So if you app uses
#' `geom_hex()` it will fail, reporting that the hexbin package is not
#' installed.
#'
#' You can overcome this problem with (e.g.) `requireNamespace(hexbin)`.
#' This will tell rsconnect that your app needs the hexbin package, without
#' otherwise affecting your code.
#'
#' @inheritParams listDeploymentFiles
#' @param appDir Directory containing application. Defaults to current working
#'   directory.
#' @return A data frame with columns:
#'   * `Package`: package name.
#'   * `Version`: local version.
#'   * `Source`: where the package was installed from.
#'   * `Repository`: for CRAN and CRAN-like repositories, the url to the
#'      repo.
#' @examples
#' \dontrun{
#'
#' # dependencies for the app in the current working dir
#' appDependencies()
#'
#' # dependencies for an app in another directory
#' appDependencies("~/projects/shiny/app1")
#' }
#' @seealso [rsconnectPackages](Using Packages with rsconnect)
#' @export
appDependencies <- function(appDir = getwd(), appFiles = NULL) {
  appFiles <- listDeploymentFiles(appDir, appFiles)
  appMetadata <- appMetadata(appDir, appFiles = appFiles)
  if (!needsR(appMetadata)) {
    return(data.frame(
      Package = character(),
      Version = character(),
      Source = character(),
      Repository = character(),
      stringsAsFactors = FALSE
    ))
  }

  bundleDir <- bundleAppDir(appDir, appFiles)
  on.exit(unlink(bundleDir, recursive = TRUE), add = TRUE)

  extraPackages <- inferRPackageDependencies(appMetadata)
  deps <- snapshotRDependencies(bundleDir, extraPackages)
  deps[c("Package", "Version", "Source", "Repository")]
}

needsR <- function(appMetadata) {
  if (appMetadata$appMode %in% c("static", "tensorflow-saved-model")) {
    return(FALSE)
  }

  # All non-Quarto content currently uses R by default.
  # Currently R is only supported by the "knitr" engine, not "jupyter" or
  # "markdown"
  is.null(appMetadata$quartoInfo) ||
    "knitr" %in% appMetadata$quartoInfo[["engines"]]
}

inferRPackageDependencies <- function(appMetadata) {
  deps <- switch(appMetadata$appMode,
    "rmd-static" = c("rmarkdown", if (appMetadata$hasParameters) "shiny"),
    "quarto-static" = "rmarkdown",
    "quarto-shiny" = c("rmarkdown", "shiny"),
    "rmd-shiny" = c("rmarkdown", "shiny"),
    "shiny" = "shiny",
    "api" = "plumber"
  )
  if (appMetadata$documentsHavePython) {
    deps <- c(deps, "reticulate")
  }
  deps
}
