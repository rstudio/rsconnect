#' Deploy a TensorFlow saved model
#'
#' This function is defunct. Posit Connect no longer supports hosting of
#' TensorFlow Model APIs. A TensorFlow model can be deployed as a
#' [Plumber API](https://tensorflow.rstudio.com/guides/deploy/plumber.html),
#' [Shiny application](https://tensorflow.rstudio.com/guides/deploy/shiny), or
#' other supported content type.
#'
#' @param modelDir Path to the saved model directory. Unused.
#' @param ... Unused.
#'
#' @family Deployment functions
#' @export
deployTFModel <- function(modelDir, ...) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "deployTFModel()",
    details = "Posit Connect no longer supports hosting of TensorFlow Model APIs."
  )
}
