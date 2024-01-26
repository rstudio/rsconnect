# Called only when the content is known to be Quarto.
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

  json <- suppressWarnings(
    system2(
      quarto, c("inspect", shQuote(appDir)),
      stdout = TRUE, stderr = TRUE
    )
  )
  status <- attr(json, "status")

  if (!is.null(status) && !is.null(appPrimaryDoc)) {
    json <- suppressWarnings(
      system2(
        quarto, c("inspect", shQuote(file.path(appDir, appPrimaryDoc))),
        stdout = TRUE, stderr = TRUE
      )
    )
    status <- attr(json, "status")
  }

  if (!is.null(status)) {
    cli::cli_abort(
      c(
        "Failed to run `quarto inspect` against your content:",
        json
      )
    )
  }
  jsonlite::fromJSON(json)
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
