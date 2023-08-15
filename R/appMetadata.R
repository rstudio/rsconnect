appMetadata <- function(appDir,
                        appFiles,
                        appPrimaryDoc = NULL,
                        quarto = NA,
                        appMode = NULL,
                        contentCategory = NULL,
                        isShinyappsServer = FALSE,
                        metadata = list()) {

  checkAppLayout(appDir, appPrimaryDoc)

  if (is_string(quarto)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "deployApp(quarto = 'can no longer be a path')",
      with = I("quarto = `TRUE` instead")
    )
    quarto <- TRUE
  } else {
    check_bool(quarto, allow_na = TRUE)
  }

  # If quarto package/IDE has supplied metadata, always use quarto
  # https://github.com/quarto-dev/quarto-r/blob/08caf0f42504e7/R/publish.R#L117-L121
  # https://github.com/rstudio/rstudio/blob/3d45a20307f650/src/cpp/session/modules/SessionRSConnect.cpp#L81-L123
  if (hasQuartoMetadata(metadata)) {
    quarto <- TRUE
  }

  if (is.null(appMode)) {
    # Generally we want to infer appPrimaryDoc from appMode, but there's one
    # special case
    if (!is.null(appPrimaryDoc) &&
          tolower(tools::file_ext(appPrimaryDoc)) == "r") {
      appMode <- "shiny"
    } else {
      # Inference only uses top-level files
      rootFiles <- appFiles[dirname(appFiles) == "."]
      appMode <- inferAppMode(
        file.path(appDir, rootFiles),
        usesQuarto = quarto,
        isShinyappsServer = isShinyappsServer
      )
    }
  }

  appPrimaryDoc <- inferAppPrimaryDoc(
    appPrimaryDoc = appPrimaryDoc,
    appFiles = appFiles,
    appMode = appMode
  )
  hasParameters <- appHasParameters(
    appDir = appDir,
    appPrimaryDoc = appPrimaryDoc,
    appMode = appMode,
    contentCategory = contentCategory
  )
  documentsHavePython <- detectPythonInDocuments(
    appDir = appDir,
    files = appFiles
  )

  if (appIsQuartoDocument(appMode)) {
    quartoInfo <- inferQuartoInfo(
      metadata = metadata,
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc
    )
  } else {
    quartoInfo <- NULL
  }

  list(
    appMode = appMode,
    appPrimaryDoc = appPrimaryDoc,
    hasParameters = hasParameters,
    contentCategory = contentCategory,
    documentsHavePython = documentsHavePython,
    quartoInfo = quartoInfo
  )
}

checkAppLayout <- function(appDir, appPrimaryDoc = NULL) {
  appFilesBase <- tolower(list.files(appDir))
  wwwFiles <- tolower(list.files(file.path(appDir, "www/")))

  primaryIsRScript <- identical(tolower(tools::file_ext(appPrimaryDoc)), "r")

  # check for single-file app collision
  if (primaryIsRScript && "app.r" %in% appFilesBase) {
    cli::cli_abort(
      "Project must not contain both {.file app.R} and a single-file Shiny app."
    )
  }

  # Do some checks for a valid application structure
  satisfiedLayouts <- c(
    shinyAndUi = all(c("server.r", "ui.r") %in% appFilesBase),
    shinyAndIndex = "server.r" %in% appFilesBase && "index.html" %in% wwwFiles,
    app = primaryIsRScript || any("app.r" %in% appFilesBase),
    Rmd = any(grepl(glob2rx("*.rmd"), appFilesBase)),
    Qmd = any(grepl(glob2rx("*.qmd"), appFilesBase)),
    static = any(grepl("(?:html?|pdf)$", appFilesBase)),
    plumber = any(c("entrypoint.r", "plumber.r") %in% appFilesBase)
  )

  if (any(satisfiedLayouts)) {
    return()
  }

  cli::cli_abort(c(
    "Cancelling deployment: invalid project layout.",
    i = "Expecting one of the following publication types:",
    " " = "1. A Shiny app with `app.R` or `server.R` + `ui.R`",
    " " = "2. R Markdown (`.Rmd`) or Quarto (`.qmd`) documents.",
    " " = "3. A website containing `.html` and/or `.pdf` files.",
    " " = "4. A plumber API with `plumber.R` or `entrypoint.R`."
  ))
}

# infer the mode of the application from files in the root dir
inferAppMode <- function(absoluteRootFiles,
                         usesQuarto = NA,
                         isShinyappsServer = FALSE) {

  matchingNames <- function(paths, pattern) {
    idx <- grepl(pattern, basename(paths), ignore.case = TRUE, perl = TRUE)
    paths[idx]
  }

  # plumber API
  plumberFiles <- matchingNames(absoluteRootFiles, "^(plumber|entrypoint).r$")
  if (length(plumberFiles) > 0) {
    return("api")
  }

  # Shiny application using single-file app.R style.
  appR <- matchingNames(absoluteRootFiles, "^app.r$")
  if (length(appR) > 0) {
    return("shiny")
  }

  rmdFiles <- matchingNames(absoluteRootFiles, "\\.rmd$")
  qmdFiles <- matchingNames(absoluteRootFiles, "\\.qmd$")

  if (is.na(usesQuarto)) {
    # Can't use _quarto.yml alone because it causes deployment failures for
    # static content: https://github.com/rstudio/rstudio/issues/11444
    quartoYml <- matchingNames(absoluteRootFiles, "^_quarto.y(a)?ml$")

    usesQuarto <- length(qmdFiles) > 0 ||
      (length(quartoYml) > 0 && length(rmdFiles > 0))
  }

  # Documents with "server: shiny" in their YAML front matter need shiny too
  hasShinyRmd <- any(sapply(rmdFiles, isShinyRmd))
  hasShinyQmd <- any(sapply(qmdFiles, isShinyRmd))

  if (hasShinyQmd) {
    return("quarto-shiny")
  } else if (hasShinyRmd) {
    if (usesQuarto) {
      return("quarto-shiny")
    } else {
      return("rmd-shiny")
    }
  }

  # Shiny application using server.R; checked later than Rmd with shiny runtime
  # because server.R may contain the server code paired with a ShinyRmd and needs
  # to be run by rmarkdown::run (rmd-shiny).
  serverR <- matchingNames(absoluteRootFiles, "^server.r$")
  if (length(serverR) > 0) {
    return("shiny")
  }

  # Any non-Shiny R Markdown or Quarto documents are rendered content and get
  # rmd-static or quarto-static.
  if (length(rmdFiles) > 0 || length(qmdFiles) > 0) {
    if (usesQuarto) {
      return("quarto-static")
    } else {
      # For shinyapps.io, treat "rmd-static" app mode as "rmd-shiny" so that
      # it can be served from a shiny process in Connect
      if (isShinyappsServer) {
        return("rmd-shiny")
      }
      return("rmd-static")
    }
  }

  # no renderable content
  "static"
}

isShinyRmd <- function(filename) {
  yaml <- yamlFromRmd(filename)
  if (is.null(yaml)) {
    return(FALSE)
  }
  is_shiny_prerendered(yaml$runtime, yaml$server)
}

yamlFromRmd <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  delim <- grep("^(---|\\.\\.\\.)\\s*$", lines)
  if (length(delim) >= 2) {
    # If at least two --- or ... lines were found...
    if (delim[[1]] == 1 || all(grepl("^\\s*$", lines[1:delim[[1]]]))) {
      # and the first is a ---
      if (grepl("^---\\s*$", lines[delim[[1]]])) {
        # ...and the first --- line is not preceded by non-whitespace...
        if (diff(delim[1:2]) > 1) {
          # ...and there is actually something between the two --- lines...
          yamlData <- paste(lines[(delim[[1]] + 1):(delim[[2]] - 1)],
                            collapse = "\n")
          return(yaml::yaml.load(yamlData))
        }
      }
    }
  }
  return(NULL)
}

# Adapted from rmarkdown:::is_shiny_prerendered()
is_shiny_prerendered <- function(runtime, server = NULL) {
  if (!is.null(runtime) && grepl("^shiny", runtime)) {
    TRUE
  } else if (identical(server, "shiny")) {
    TRUE
  } else if (is.list(server) && identical(server[["type"]], "shiny")) {
    TRUE
  } else {
    FALSE
  }
}

# If deploying an R Markdown, Quarto, or static content, infer a primary
# document if one is not already specified.
# Note: functionality in inferQuartoInfo() depends on primary doc inference
# working the same across app modes.
inferAppPrimaryDoc <- function(appPrimaryDoc, appFiles, appMode) {
  if (!is.null(appPrimaryDoc)) {
    return(appPrimaryDoc)
  }

  # Only documents and static apps don't have primary _doc_
  if (!(appIsDocument(appMode) || appMode == "static")) {
    return(appPrimaryDoc)
  }

  # determine expected primary document extension
  ext <- if (appMode == "static") "\\.html?$" else "\\.[Rq]md$"

  # use index file if it exists
  matching <- grepl(paste0("^index", ext), appFiles, ignore.case = TRUE)
  if (!any(matching)) {
    # no index file found, so pick the first one we find
    matching <- grepl(ext, appFiles, ignore.case = TRUE)

    if (!any(matching)) {
      cli::cli_abort(c(
        "Failed to determine {.arg appPrimaryDoc}.",
        x = "No files matching {.str {ext}}."
      ))
    }
  }

  if (sum(matching) > 1) {
    # if we have multiple matches, pick the first
    appFiles[matching][[1]]
  } else {
    appFiles[matching]
  }
}

appIsDocument <- function(appMode) {
  appMode %in% c(
    "rmd-static",
    "rmd-shiny",
    "quarto-static",
    "quarto-shiny"
  )
}

appIsQuartoDocument <- function(appMode) {
  appMode %in% c(
    "quarto-static",
    "quarto-shiny"
  )
}


appHasParameters <- function(appDir, appPrimaryDoc, appMode, contentCategory = NULL) {
  # Only Rmd deployments are marked as having parameters. Shiny applications
  # may distribute an Rmd alongside app.R, but that does not cause the
  # deployment to be considered parameterized.
  #
  # https://github.com/rstudio/rsconnect/issues/246
  if (!(appIsDocument(appMode))) {
    return(FALSE)
  }
  # Sites don't ever have parameters
  if (identical(contentCategory, "site")) {
    return(FALSE)
  }

  # Only Rmd files have parameters.
  if (tolower(tools::file_ext(appPrimaryDoc)) == "rmd") {
    filename <- file.path(appDir, appPrimaryDoc)
    yaml <- yamlFromRmd(filename)
    if (!is.null(yaml)) {
      params <- yaml[["params"]]
      # We don't care about deep parameter processing, only that they exist.
      return(!is.null(params) && length(params) > 0)
    }
  }
  FALSE
}

detectPythonInDocuments <- function(appDir, files = NULL) {
  if (is.null(files)) {
    # for testing
    files <- bundleFiles(appDir)
  }

  rmdFiles <- grep("^[^/\\\\]+\\.[rq]md$", files, ignore.case = TRUE, perl = TRUE, value = TRUE)
  for (rmdFile in rmdFiles) {
    if (documentHasPythonChunk(file.path(appDir, rmdFile))) {
      return(TRUE)
    }
  }
  return(FALSE)
}
documentHasPythonChunk <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  matches <- grep("`{python", lines, fixed = TRUE)
  return(length(matches) > 0)
}
