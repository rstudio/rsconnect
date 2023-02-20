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

# quarto ------------------------------------------------------------------

quartoPathOrSkip <- function() {
  skip_on_cran()
  quarto <- quarto_path()
  skip_if(is.null(quarto), "quarto cli is not installed")
  return(quarto)
}


quarto_path <- function() {
  path_env <- Sys.getenv("QUARTO_PATH", unset = NA)
  if (!is.na(path_env)) {
    return(path_env)
  } else {
    locations <- c(
      "quarto", # Use PATH
      "/usr/local/bin/quarto", # Location used by some installers
      "/opt/quarto/bin/quarto", # Location used by some installers
      "/Applications/RStudio.app/Contents/MacOS/quarto/bin/quarto" # macOS IDE
    )
    for (location in locations) {
      path <- unname(Sys.which(location))
      if (nzchar(path)) return(path)
    }
    return(NULL)
  }
}

