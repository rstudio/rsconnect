appMetadata <- function(appDir,
                        appFiles = NULL,
                        appPrimaryDoc = NULL,
                        quarto = NULL,
                        contentCategory = NULL,
                        isCloudServer = FALSE,
                        metadata = list()) {

  if (is.null(appFiles)) {
    appFiles <- bundleFiles(appDir)
  }

  if (!is.null(contentCategory)) {
    assetTypeName <- contentCategory
  } else if (!is.null(appPrimaryDoc)) {
    assetTypeName <- "document"
  } else {
    assetTypeName <- "application"
  }

  quartoInfo <- inferQuartoInfo(
    appDir = appDir,
    appPrimaryDoc = appPrimaryDoc,
    appFiles = appFiles,
    quarto = quarto,
    metadata = metadata
  )
  appMode <- inferAppMode(
    appDir = appDir,
    appPrimaryDoc = appPrimaryDoc,
    files = appFiles,
    quartoInfo = quartoInfo,
    isCloudServer = isCloudServer
  )
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

  list(
    assetTypeName = assetTypeName,
    appMode = appMode,
    appPrimaryDoc = appPrimaryDoc,
    hasParameters = hasParameters,
    documentsHavePython = documentsHavePython,
    quartoInfo = quartoInfo
  )
}

# infer the mode of the application from its layout
# unless we're an API, in which case, we're API mode.
inferAppMode <- function(appDir, appPrimaryDoc, files, quartoInfo, isCloudServer = FALSE) {
  # plumber API
  plumberFiles <- grep("^(plumber|entrypoint).r$", files, ignore.case = TRUE, perl = TRUE)
  if (length(plumberFiles) > 0) {
    return("api")
  }

  # single-file Shiny application
  if (!is.null(appPrimaryDoc) &&
      tolower(tools::file_ext(appPrimaryDoc)) == "r") {
    return("shiny")
  }

  # Shiny application using single-file app.R style.
  appR <- grep("^app.r$", files, ignore.case = TRUE, perl = TRUE)
  if (length(appR) > 0) {
    return("shiny")
  }

  # Determine if we have Rmd files, and if they use the Shiny runtime.
  rmdFiles <- grep("^[^/\\\\]+\\.rmd$", files, ignore.case = TRUE, perl = TRUE, value = TRUE)
  shinyRmdFiles <- sapply(file.path(appDir, rmdFiles), isShinyRmd)

  # Determine if we have qmd files, and if they use the Shiny runtime
  qmdFiles <- grep("^[^/\\\\]+\\.qmd$", files, ignore.case = TRUE, perl = TRUE, value = TRUE)
  shinyQmdFiles <- sapply(file.path(appDir, qmdFiles), isShinyRmd)

  # We make Quarto requirement conditional on the presence of files that Quarto
  # can render and _quarto.yml, because keying off the presence of qmds
  # *or* _quarto.yml was causing deployment failures in static content.
  # https://github.com/rstudio/rstudio/issues/11444
  hasQuartoYaml <- any(grepl("^_quarto.y(a)?ml$", x = files, ignore.case = TRUE, perl = TRUE))
  hasQuartoCompatibleFiles <- any(length(qmdFiles) > 0, length(rmdFiles > 0))
  requiresQuarto <- (hasQuartoCompatibleFiles && hasQuartoYaml) || length(qmdFiles) > 0

  # We gate the deployment of content that appears to be Quarto behind the
  # presence of Quarto metadata. Rmd files can still be deployed as Quarto
  # content.
  if (requiresQuarto && is.null(quartoInfo)) {
    stop(paste(
      "Attempting to deploy Quarto content without Quarto metadata.",
      "Please provide the path to a quarto binary to the 'quarto' argument."
    ))
  }

  # Shiny or Quarto documents with "server: shiny" in their YAML front matter
  # are rmd-shiny or quarto-shiny.
  if (any(shinyRmdFiles) || any(shinyQmdFiles)) {
    if (!is.null(quartoInfo)) {
      return("quarto-shiny")
    } else {
      return("rmd-shiny")
    }
  }

  # Shiny application using server.R; checked later than Rmd with shiny runtime
  # because server.R may contain the server code paired with a ShinyRmd and needs
  # to be run by rmarkdown::run (rmd-shiny).
  serverR <- grep("^server.r$", files, ignore.case = TRUE, perl = TRUE)
  if (length(serverR) > 0) {
    return("shiny")
  }

  # Any non-Shiny R Markdown or Quarto documents are rendered content and get
  # rmd-static or quarto-static.
  if (length(rmdFiles) > 0 || length(qmdFiles) > 0) {
    if (!is.null(quartoInfo)) {
      return("quarto-static")
    } else {
      # For Shinyapps and posit.cloud, treat "rmd-static" app mode as "rmd-shiny" so that
      # they can be served from a shiny process in Connect until we have better support of
      # rmarkdown static content
      if (isCloudServer) {
        return("rmd-shiny")
      }
      return("rmd-static")
    }
  }

  # We don't have an RMarkdown, Shiny app, or Plumber API, but we have a saved model
  if (length(grep("(saved_model.pb|saved_model.pbtxt)$", files, ignore.case = TRUE, perl = TRUE)) > 0) {
    return("tensorflow-saved-model")
  }

  # no renderable content here; if there's at least one file, we can just serve
  # it as static content
  if (length(files) > 0) {
    return("static")
  }

  # there doesn't appear to be any content here we can use
  stop("No content to deploy; cannot detect content type.")
}

# If deploying an R Markdown, Quarto, or static content, infer a primary
# document if one is not already specified.
  # Note: functionality in inferQuartoInfo() depends on primary doc inference
  # working the same across app modes.
inferAppPrimaryDoc <- function(appPrimaryDoc, appFiles, appMode) {
  if (!is.null(appPrimaryDoc)) {
    return(appPrimaryDoc)
  }

  # Non-document apps don't have primary _doc_
  if (appMode %in% c("shiny", "api")) {
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

appHasParameters <- function(appDir, appPrimaryDoc, appMode, contentCategory) {
  # Only Rmd deployments are marked as having parameters. Shiny applications
  # may distribute an Rmd alongside app.R, but that does not cause the
  # deployment to be considered parameterized.
  #
  # https://github.com/rstudio/rsconnect/issues/246
  parameterAppModes <- c(
      "rmd-static",
      "rmd-shiny",
      "quarto-static",
      "quarto-shiny"
  )
  if (!(appMode %in% parameterAppModes)) {
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

isShinyRmd <- function(filename) {
  yaml <- yamlFromRmd(filename)
  if (!is.null(yaml)) {
    runtime <- yaml[["runtime"]]
    server <- yaml[["server"]]
    if (!is.null(runtime) && grepl("^shiny", runtime)) {
      # ...and "runtime: shiny", then it's a dynamic Rmd.
      return(TRUE)
    } else if (!is.null(server)) {
      if (identical(server, "shiny")) {
        return(TRUE)
      } else if (is.list(server) && identical(server[["type"]], "shiny")) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

detectPythonInDocuments <- function(appDir, files) {
  rmdFiles <- grep("^[^/\\\\]+\\.[rq]md$", files, ignore.case = TRUE, perl = TRUE,
                   value = TRUE)

  if (length(rmdFiles) > 0) {
    for (rmdFile in rmdFiles) {
      if (documentHasPythonChunk(file.path(appDir, rmdFile))) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

documentHasPythonChunk <- function(filename) {
  lines <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  matches <- grep("`{python", lines, fixed = TRUE)
  return(length(matches) > 0)
}



# Attempt to gather Quarto version and engines, first from quarto inspect if a
# quarto executable is provided, and then from metadata.
inferQuartoInfo <- function(appDir, appPrimaryDoc, appFiles, quarto, metadata) {
  quartoInfo <- NULL
  if (!is.null(metadata$quarto_version)) {
    # Prefer metadata, because that means someone already ran quarto inspect
    quartoInfo <- list(
      "version" = metadata[["quarto_version"]],
      "engines" = metadata[["quarto_engines"]]
    )
  }
  if (is.null(quartoInfo) && !is.null(quarto)) {
    # If we don't yet have Quarto details, run quarto inspect ourselves

    # If no appPrimaryDoc has been provided, we will use the file that will be
    # chosen if this deployment ends up with an R Markdown or Quarto app mode.
    # This works because:

    # - App modes are only used to gate primary doc inference; the behavior does
    #   not differ between app modes.
    # - inferAppPrimaryDoc() returns appPrimaryDoc() if it is not null.
    tryCatch({
      appPrimaryDoc <- inferAppPrimaryDoc(
        appPrimaryDoc = appPrimaryDoc,
        appFiles = appFiles,
        appMode = "quarto-static"
      )
    }, error = function(e) {}
    )
    inspect <- quartoInspect(
      appDir = appDir,
      appPrimaryDoc = appPrimaryDoc,
      quarto = quarto
    )
    if (!is.null(inspect)) {
      quartoInfo <- list(
        version = inspect[["quarto"]][["version"]],
        engines = I(inspect[["engines"]])
      )
    }
  }
  return(quartoInfo)
}


# Run "quarto inspect" on the target and returns its output as a parsed object.
quartoInspect <- function(appDir = NULL, appPrimaryDoc = NULL, quarto = NULL) {
  if (is.null(quarto)) {
    return(NULL)
  }
  inspect <- NULL
  # If "quarto inspect appDir" fails, we will try "quarto inspect
  # appPrimaryDoc", so that we can support single files as well as projects.
  primaryDocPath <- file.path(appDir, appPrimaryDoc) # prior art: appHasParameters()
  for (path in c(appDir, primaryDocPath)) {
    args <- c("inspect", path.expand(path))
    tryCatch(
      {
        inspectOutput <- suppressWarnings(system2(quarto, args, stdout = TRUE, stderr = TRUE))
        inspect <- jsonlite::fromJSON(inspectOutput)
      },
      error = function(e) e
    )
    if (!is.null(inspect)) break
  }
  return(inspect)
}

appUsesR <- function(quartoInfo) {
  if (is.null(quartoInfo)) {
    # All non-Quarto content currently uses R by default.
    # To support non-R content in rsconnect, we could inspect appmode here.
    return(TRUE)
  }
  # R is used only supported with the "knitr" engine, not "jupyter" or "markdown"
  # Technically, "jupyter" content could support R.
  return("knitr" %in% quartoInfo[["engines"]])
}

appUsesPython <- function(quartoInfo) {
  if (is.null(quartoInfo)) {
    # No R-based, non-Quarto content uses Python by default.
    # Looking for Python chunks in Rmd needs to happen separately.
    return(FALSE)
  }
  # Python is a direct consequence of the "jupyter" engine; not "knitr" or "markdown".
  return("jupyter" %in% quartoInfo[["engines"]])
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
