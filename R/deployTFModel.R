#' Deploy a TensorFlow saved model
#'
#' Deploys a directory containing a Tensorflow saved model file. A saved model directory might look like this:
#' \preformatted{
#' ./1/
#' ./1/saved_model.pb or ./1/saved_model.pbtxt
#' ./1/variables/
#' ./1/variables/variables.data-00000-of-00001
#' ./1/variables/variables.index
#' }
#' For information on creating saved models, see the Keras method
#' \code{\link[keras]{export_savedmodel.keras.engine.training.Model}} or the TensorFlow method
#' \code{\link[tensorflow]{export_savedmodel}}. If using the TensorFlow package for R, the official
#' TensorFlow guide for saving and restoring models may be useful:
#' \url{https://www.tensorflow.org/programmers_guide/saved_model#overview_of_saving_and_restoring_models}
#'
#' @param modelDir Path to the saved model directory. MUST contain \emph{saved_model.pb} or
#'   \emph{saved_model.pbtxt}
#' @param ... Additional arguments to \code{\link{deployApp}}.
#'
#' @details Deploy a single Tensorflow saved model as a bundle. Should be passed a directory that contains the
#'   \emph{saved_model.pb} or \emph{saved_model.pbtxt} file, as well as any variables and assets
#'   necessary to load the model.
#'
#' @export
deployTFModel <- function(modelDir,
                          ...) {
  if (!file.exists(modelDir)) {
    stop("The model directory at '", modelDir, "' does not exist.")
  }
  if (!utils::file_test("-d", modelDir)) {
    stop("The modelDir at '", modelDir, "' is not a directory.")
  }
  # Checking for saved_model.pb is done in `lint-framework.R`
  deployApp(appDir = modelDir,
            contentCategory = 'tensorflow-saved-model',
            ...)
}
