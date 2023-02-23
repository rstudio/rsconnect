#' Deploy a Plumber API
#'
#' Deploys an application consisting of plumber API routes. The given directory
#' must contain a script returning a `plumb` object or a plumber API definition.
#'
#' @param api Path to the API project directory. Must contain either
#'   `entrypoint.R` or `plumber.R`
#' @param ... Additional arguments to [deployApp()].
#'
#' @details Deploy a plumber API definition by either supplying a directory
#'   containing `plumber.R` (an API definition) or `entrypoint.R` that returns a
#'   `plumb` object created by `plumber::plumb()`. See the plumber documentation
#'   for more information.
#'
#' @family Deployment functions
#' @export
deployAPI <- function(api, ...) {
  check_installed(
    "plumber",
    version = "0.3.2",
    reason = "to deploy plumber APIs"
  )

  check_directory(api)

  # Checking for entrypoint.R or plumber.R is done in `lint-framework.R`
  deployApp(appDir = api, ...)
}
