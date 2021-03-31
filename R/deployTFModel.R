#' Deploy a TensorFlow saved model
#'
#' Deploys a directory containing a Tensorflow saved model file.
#'
#' Deploy a single Tensorflow saved model as a bundle. Should be passed a
#' directory that contains the *saved_model.pb* or *saved_model.pbtxt* file,
#' as well as any variables and assets necessary to load the model.
#'
#' A saved model directory might look like this:
#'
#' ```
#' ./1/
#' ./1/saved_model.pb or ./1/saved_model.pbtxt
#' ./1/variables/
#' ./1/variables/variables.data-00000-of-00001
#' ./1/variables/variables.index
#' ```
#'
#' For information on creating saved models, see the Keras method
#' [keras::export_savedmodel.keras.engine.training.Model()] or the TensorFlow
#' method [tensorflow::export_savedmodel()]. If using the TensorFlow package for
#' R, the official [TensorFlow guide for saving and restoring models](
#' <https://www.tensorflow.org/guide/saved_model>)
#' may be useful.
#'
#' @param modelDir Path to the saved model directory. MUST contain
#'   *saved_model.pb* or *saved_model.pbtxt*
#' @param ... Additional arguments to [deployApp()].
#'
#' @references <https://www.tensorflow.org/guide/saved_model>
#'
#'
#' @details
#'
#' @family Deployment functions
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
  deployApp(appDir = modelDir, ...)
}
