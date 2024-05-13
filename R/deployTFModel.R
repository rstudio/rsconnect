#' Deploy a TensorFlow saved model
#'
#' Deploys a directory containing a TensorFlow saved model.
#'
#' #' @inheritParams deployApp
#'
#' @family Deployment functions
#' @export
deployTFModel <- function(...) {
  deployApp(appMode = "tensorflow-saved-model", ...)
}
