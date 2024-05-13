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
    # special case: RStudio provides appPrimaryDoc when deploying Shiny
    # applications. They may have name.R, not app.R or server.R.
    #
    # This file is later renamed to app.R when deployed by bundleAppDir().
    if (!is.null(appPrimaryDoc) &&
          tolower(tools::file_ext(appPrimaryDoc)) == "r") {
      appMode <- "shiny"
    } else {
      # Inference only uses top-level files
      appMode <- inferAppMode(
        appDir,
        appFiles,
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
  primaryIsRScript <- identical(tolower(tools::file_ext(appPrimaryDoc)), "r")

  # check for single-file app collision
  if (primaryIsRScript && "app.r" %in% appFilesBase) {
    cli::cli_abort(
      "Project must not contain both {.file app.R} and a single-file Shiny app."
    )
  }

  # all other layouts are allowed; the server determines (with the required packages) if the content
  # can be run/served.
}

# Infer the mode of the application from included files. Most content types
# only consider files at the directory root. TensorFlow saved models may be
# anywhere in the hierarchy.
inferAppMode <- function(
  appDir,
  appFiles,
  usesQuarto = NA,
  isShinyappsServer = FALSE) {

  rootFiles <- appFiles[dirname(appFiles) == "."]
  absoluteRootFiles <- file.path(appDir, rootFiles)
  absoluteAppFiles <- file.path(appDir, appFiles)

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
  hasRmd <- length(rmdFiles) > 0
  qmdFiles <- matchingNames(absoluteRootFiles, "\\.qmd$")
  hasQmd <- length(qmdFiles) > 0
  rFiles <- matchingNames(absoluteRootFiles, "\\.r$")
  hasR <- length(rFiles) > 0
  quartoYml <- matchingNames(absoluteRootFiles, "^_quarto.y(a)?ml$")
  hasQuartoYml <- length(quartoYml) > 0

  if (is.na(usesQuarto)) {
    # Determine if the incoming content implies the need for Quarto.
    #
    # *.qmd files are enough of an indication by themselves.
    # *.rmd and *.r files need a _quarto.yml file to emphasize the need for Quarto.
    #
    # Do not rely on _quarto.yml alone, as RStudio includes that file even when
    # publishing HTML. https://github.com/rstudio/rstudio/issues/11444
    usesQuarto <- (
      hasQmd ||
      (hasQuartoYml && hasRmd) ||
      (hasQuartoYml && hasR)
    )
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
  # because server.R may contain the server code paired with a ShinyRmd and
  # needs to be run by rmarkdown::run (rmd-shiny).
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

  if (hasR) {
    # We have R scripts but it was not otherwise identified as Shiny or Plumber
    # and also not accompanied by *.qmd or *.rmd files.
    #
    # Assume that this is a rendered script, as this is a better fall-back than
    # "static".
    return("quarto-static")
  }

  # TensorFlow model files are lower in the hierarchy, not at the root.
  modelFiles <- matchingNames(absoluteAppFiles, "^(saved_model.pb|saved_model.pbtxt)$")
  if (length(modelFiles) > 0) {
    return("tensorflow-saved-model")
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
  ext <- switch(appMode,
                "static"        = "\\.html?$",
                "quarto-static" = "\\.(r|rmd|qmd)",
                "quarto-shiny"  = "\\.(rmd|qmd)",
                "\\.rmd$")

  # use index file if it exists
  matching <- grepl(paste0("^index", ext), appFiles, ignore.case = TRUE)
  if (!any(matching)) {
    # no index file found, so pick the first one we find
    matching <- grepl(ext, appFiles, ignore.case = TRUE)

    if (!any(matching)) {
      cli::cli_abort(c(
        "Failed to determine {.arg appPrimaryDoc} for {.str {appMode}} content.",
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
