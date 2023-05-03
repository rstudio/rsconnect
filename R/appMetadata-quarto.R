inferQuartoInfo <- function(metadata, appDir, appPrimaryDoc) {
  if (hasQuartoMetadata(metadata)) {
    return(list(
      version = metadata[["quarto_version"]],
      engines = metadata[["quarto_engines"]]
    ))
  }

  # If we don't yet have Quarto details, run quarto inspect ourselves
  inspect <- quartoInspect(
    appDir = appDir,
    appPrimaryDoc = appPrimaryDoc
  )
  if (is.null(inspect)) {
    return(NULL)
  }

  list(
    version = inspect[["quarto"]][["version"]],
    engines = I(inspect[["engines"]])
  )
}

hasQuartoMetadata <- function(x) {
  !is.null(x$quarto_version)
}

# Run "quarto inspect" on the target and returns its output as a parsed object.
quartoInspect <- function(appDir = NULL, appPrimaryDoc = NULL) {
  # If "quarto inspect appDir" fails, we will try "quarto inspect
  # appPrimaryDoc", so that we can support single files as well as projects.
  quarto <- quarto_path()
  if (is.null(quarto)) {
    cli::cli_abort(c(
      "`quarto` not found.",
      i = "Check that it is installed and available on your {.envvar PATH}."
    ))
  }

  paths <- c(appDir, file.path(appDir, appPrimaryDoc))

  for (path in paths) {
    args <- c("inspect", path.expand(path))
    inspect <- tryCatch(
      {
        json <- suppressWarnings(system2(quarto, args, stdout = TRUE, stderr = TRUE))
        parsed <- jsonlite::fromJSON(json)
        return(parsed)
      },
      error = function(e) NULL
    )
  }
  return(NULL)
}

# inlined from quarto::quarto_path()
quarto_path <- function() {
  path_env <- Sys.getenv("QUARTO_PATH", unset = NA)
  if (is.na(path_env)) {
    path <- unname(Sys.which("quarto"))
    if (nzchar(path)) {
      path
    } else {
      NULL
    }
  } else {
    path_env
  }
}
