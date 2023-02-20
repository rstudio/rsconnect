# Create anonymous function that we can later call to get all needed python
# metdata for the manifest
pythonConfigurator <- function(python,
                               forceGenerate = FALSE,
                               condaMode = FALSE) {

  if (is.null(python)) {
    return(NULL)
  }

  force(condaMode)
  force(forceGenerate)

  function(appDir) {
    withCallingHandlers(
      inferPythonEnv(
        appDir,
        python = python,
        condaMode = condaMode,
        forceGenerate = forceGenerate
      ),
      error = function(err) {
        cli::cli_abort(
          "Failed to detect python environment",
          parent = err
        )
      }
    )
  }
}

appUsesPython <- function(quartoInfo) {
  if (is.null(quartoInfo)) {
    # No R-based, non-Quarto content uses Python by default.
    # Looking for Python chunks in Rmd needs to happen separately.
    FALSE
  } else {
    # Python is a direct consequence of the "jupyter" engine; not "knitr" or "markdown".
    "jupyter" %in% quartoInfo[["engines"]]
  }
}

getPythonForTarget <- function(path, accountDetails) {
  # python is enabled on Connect and posit.cloud, but not on Shinyapps
  targetIsShinyapps <- isShinyappsServer(accountDetails$server)
  pythonEnabled <- getOption(
    "rsconnect.python.enabled",
    default = !targetIsShinyapps
  )
  if (pythonEnabled) {
    getPython(path)
  } else {
    NULL
  }
}

getPython <- function(path = NULL) {
  if (is.null(path)) {
    path <- Sys.getenv("RETICULATE_PYTHON", unset = Sys.getenv("RETICULATE_PYTHON_FALLBACK"))
    if (path == "") {
      return(NULL)
    }
  }
  path.expand(path)
}

inferPythonEnv <- function(workdir,
                           python = getPython(),
                           condaMode = FALSE,
                           forceGenerate = FALSE) {
  # run the python introspection script
  env_py <- system.file("resources/environment.py", package = "rsconnect")
  args <- c(
    shQuote(env_py),
    if (condaMode) "-c",
    if (forceGenerate) "-f",
    shQuote(workdir)
  )

  # First check for reticulate. Then see if python is loaded in reticulate
  # space, verify anaconda presence, and verify that the user hasn't specified
  # that they don't want their conda environment captured.
  hasConda <- is_installed("reticulate") &&
    reticulate::py_available(initialize = FALSE) &&
    reticulate::py_config()$anaconda &&
    !condaMode

  if (hasConda) {
    prefix <- getCondaEnvPrefix(python)
    conda <- getCondaExeForPrefix(prefix)
    args <- c("run", "-p", prefix, python, args)
    # conda run -p <prefix> python inst/resources/environment.py <flags> <dir>
    output <- system2(command = conda, args = args, stdout = TRUE, stderr = NULL, wait = TRUE)
  } else {
    output <- system2(command = python, args = args, stdout = TRUE, stderr = NULL, wait = TRUE)
  }

  environment <- jsonlite::fromJSON(output)
  if (is.null(environment$error)) {

    list(
      version = environment$python,
      package_manager = list(
        name = environment$package_manager,
        version = environment[[environment$package_manager]],
        package_file = environment$filename,
        contents = environment$contents
      )
    )
  } else {
    cli::cli_abort(environment$error)
  }
}

getCondaEnvPrefix <- function(python) {
  prefix <- dirname(dirname(python))
  if (!file.exists(file.path(prefix, "conda-meta"))) {
    stop(paste("Python from", python, "does not look like a conda environment: cannot find `conda-meta`"))
  }
  prefix
}

getCondaExeForPrefix <- function(prefix) {
  miniconda <- dirname(dirname(prefix))
  conda <- file.path(miniconda, "bin", "conda")
  if (isWindows()) {
    conda <- paste(conda, ".exe", sep = "")
  }
  if (!file.exists(conda)) {
    stop(paste("Conda env prefix", prefix, "does not have the `conda` command line interface."))
  }
  conda
}

