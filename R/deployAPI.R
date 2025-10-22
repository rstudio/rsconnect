#' Deploy a Plumber API
#'
#' @description
#' Deploys an application consisting of plumber API routes. The given directory
#' must contain a script returning a `plumb` object or a plumber API definition.
#'
#' Supported servers: Posit Connect and ShinyApps servers
#'
#' @param api Path to the API project directory. Must contain either
#'   `entrypoint.R` or `plumber.R` (for plumber APIs) or `_server.yml` (for
#'   plumber2 APIs)
#' @param ... Additional arguments to [deployApp()].
#'
#' @details Deploy a plumber API definition by either supplying a directory
#'   containing `plumber.R` (an API definition) or `entrypoint.R` that returns a
#'   `plumb` object created by `plumber::plumb()`. See the plumber documentation
#'   for more information. Alternatively, deploy a plumber2 API by supplying a
#'   directory containing `_server.yml`.
#'
#' @family Deployment functions
#' @export
deployAPI <- function(api, ...) {
  check_directory(api)

  # Checking for entrypoint.R or plumber.R is done in `lint-framework.R`
  deployApp(appDir = api, ...)
}
