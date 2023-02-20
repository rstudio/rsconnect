pythonPathOrSkip <- function() {
  skip_if_not_installed("reticulate")

  path <- getPython()
  if (is.null(path)) {
    skip("`getPython()` can't find python")
  }

  path <- reticulate::py_config()$python

  pipMissing <- system2(path, "-m pip help", stdout = NULL, stderr = NULL)
  if (pipMissing != 0) {
    skip("pip is not installed")
  }

  path
}
