#' Deploy a Tensorflow saved model 
#'
#' Deploys a directory containing a Tensorflow saved model file. A saved model directory might look like this:
#' `./1/`
#' `./1/saved_model.pb`
#' `./1/variables/`
#' `./1/variables/variables.data-00000-of-00001`
#' `./1/variables/variables.index`
#'
#' @param modelDir Path to the saved model directory. MUST contain `saved_model.pb`
#' @param ... Additional arguments to \code{\link{deployApp}}.
#'
#' @details Deploy a single Tensorflow saved model as a bundle. Should be passed a directory that contains the 
#' `saved_model.pb` file, as well as any variables and assets necessary to load the model.
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