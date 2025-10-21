#' Deploy a TensorFlow saved model
#'
#' @description
#' Supported servers: Posit Connect and ShinyApps servers
#'
#' Deploys a directory containing a TensorFlow saved model.
#'
#' @param ... Additional arguments to [deployApp()].
#'
#' @family Deployment functions
#' @export
deployTFModel <- function(...) {
  deployApp(appMode = "tensorflow-saved-model", ...)
}
