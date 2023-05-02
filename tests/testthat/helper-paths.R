pythonPathOrSkip <- function() {
  skip_if_not_installed("reticulate")

  if (!reticulate::py_available(TRUE)) {
    skip("python not found by reticulate")
  }
  path <- reticulate::py_config()$python

  pipMissing <- system2(path, "-m pip help", stdout = NULL, stderr = NULL)
  if (pipMissing != 0) {
    skip("pip is not installed")
  }

  path
}

# quarto ------------------------------------------------------------------

skip_if_no_quarto <- function() {
  quarto <- quarto_path()
  skip_if(is.null(quarto), "quarto cli is not installed")

  invisible()
}
