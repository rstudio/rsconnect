#' Deploy a TensorFlow saved model
#'
#' @description
#' Deploys a directory containing a TensorFlow saved model.
#'
#' Supported servers: Posit Connect and ShinyApps servers
#'
#' @param ... Additional arguments to [deployApp()].
#'
#' @family Deployment functions
#' @export
deployTFModel <- function(...) {
  deployApp(appMode = "tensorflow-saved-model", ...)
}
