# Create anonymous function that we can later call to get all needed python
# metdata for the manifest
pythonConfigurator <- function(python, forceGenerate = FALSE) {
  if (is.null(python)) {
    return(NULL)
  }

  force(forceGenerate)

  function(appDir) {
    withCallingHandlers(
      inferPythonEnv(
        appDir,
        python = python,
        forceGenerate = forceGenerate
      ),
      error = function(err) {
        cli::cli_abort(
          "Failed to detect python environment using {.val {python}}",
          parent = err
        )
      }
    )
  }
}

# python is enabled on Connect, but not on Shinyapps
getPythonForTarget <- function(path, accountDetails) {
  targetIsShinyapps <- isShinyappsServer(accountDetails$server)
  pythonEnabled <- getOption("rsconnect.python.enabled", !targetIsShinyapps)
  if (pythonEnabled) {
    getPython(path)
  } else {
    NULL
  }
}

getPython <- function(path = NULL) {
  if (!is.null(path)) {
    return(path.expand(path))
  }

  path <- Sys.getenv("RETICULATE_PYTHON")
  if (path != "") {
    return(path.expand(path))
  }

  path <- Sys.getenv("RETICULATE_PYTHON_FALLBACK")
  if (path != "") {
    return(path.expand(path))
  }

  NULL
}

inferPythonEnv <- function(
  workdir,
  python = getPython(),
  forceGenerate = FALSE
) {
  # run the python introspection script
  env_py <- system.file("resources/environment.py", package = "rsconnect")
  args <- c(
    shQuote(env_py),
    if (forceGenerate) "-f",
    shQuote(workdir)
  )

  hasConda <- is_installed("reticulate") &&
    reticulate::py_available(initialize = FALSE) &&
    reticulate::py_config()$anaconda

  if (hasConda) {
    prefix <- getCondaEnvPrefix(python)
    conda <- getCondaExeForPrefix(prefix)
    args <- c("run", "-p", prefix, python, args)
    # conda run -p <prefix> python inst/resources/environment.py <flags> <dir>
    output <- system2(
      command = conda,
      args = args,
      stdout = TRUE,
      stderr = NULL,
      wait = TRUE
    )
  } else {
    output <- system2(
      command = python,
      args = args,
      stdout = TRUE,
      stderr = NULL,
      wait = TRUE
    )
  }

  environment <- jsonlite::fromJSON(sanitizeSystem2json(output))
  if (!is.null(environment$warning)) {
    warning(environment$warning)
  }
  if (is.null(environment$error)) {
    list(
      version = environment$python,
      requires = environment$requires,
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
    stop(paste(
      "Python from",
      python,
      "does not look like a conda environment: cannot find `conda-meta`"
    ))
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
    stop(paste(
      "Conda env prefix",
      prefix,
      "does not have the `conda` command line interface."
    ))
  }
  conda
}
