##' Lint a Project
##'
##' Takes the set of active linters (see [addLinter()]), and applies
##' them to all files within a project.
##'
##' @param project Path to a project directory.
##' @param files Specific files to lint. Can be NULL, in which case all
##'   the files in the directory will be linted.
##' @param appPrimaryDoc The primary file in the project directory. Can be NULL,
##'   in which case it's inferred (if possible) from the directory contents.
##' @export
lint <- function(project, files = NULL, appPrimaryDoc = NULL) {
  check_directory(project)
  files <- listDeploymentFiles(project, files)

  # Perform actions within the project directory (so relative paths are easily used)
  owd <- getwd()
  defer(setwd(owd))
  setwd(project)

  linters <- mget(objects(.__LINTERS__.), envir = .__LINTERS__.)

  # Identify all files that will be read in by one or more linters
  projectFilesToLint <- Reduce(union, lapply(linters, function(linter) {
    getLinterApplicableFiles(linter, files)
  }))

  # Read in the files
  encoding <- activeEncoding(project)
  projectContent <- lapply(projectFilesToLint, function(file) {

    # force native encoding (disable any potential internal conversion)
    old <- options(encoding = "native.enc")
    defer(options(old))

    # read content with requested encoding
    # TODO: may consider converting from native encoding to UTF-8 if appropriate
    readLines(file, encoding = encoding, warn = FALSE)

  })

  names(projectContent) <- projectFilesToLint
  lintResults <- vector("list", length(linters))
  names(lintResults) <- names(linters)

  ## Apply each linter
  for (i in seq_along(linters)) {
    linter <- linters[[i]]
    applicableFiles <- getLinterApplicableFiles(linter, projectFilesToLint)
    lintIndices <- vector("list", length(applicableFiles))
    names(lintIndices) <- applicableFiles

    ## Apply linter to each file
    for (j in seq_along(applicableFiles)) {
      file <- applicableFiles[[j]]
      tryCatch(
        expr = {
          lintIndices[[j]] <- applyLinter(linter,
                                          projectContent[[file]],
                                          project = project,
                                          path = file,
                                          files = files)
        },
        error = function(e) {
          message("Failed to lint file '", file, "'")
          message("The linter failed with message:\n")
          message(e$message)
          lintIndices[[j]] <- numeric(0)
        }
      )
    }

    ## Get the messages associated with each lint
    lintMessages <- enumerate(lintIndices, function(x, i) {
      if (length(x)) {
        message <- linter$message
        if (is.function(message)) {
          linter$message(projectContent[[names(lintIndices)[i]]], x)
        } else {
          makeLinterMessage(message, projectContent[[names(lintIndices)[i]]], x)
        }
      } else {
        character()
      }
    })

    ## Assign the result
    lintResults[[i]] <- list(
      files = applicableFiles,
      indices = lintIndices,
      message = lintMessages,
      suggestion = linter$suggestion
    )

  }

  ## Get all of the linted files, and transform the results into a by-file format
  lintedFiles <- Reduce(union, lapply(lintResults, function(x) {
    names(x$indices)
  }))

  lintFields <- c("indices", "message")
  fileResults <- lapply(lintedFiles, function(file) {
    result <- lapply(lintResults, function(result) {
      subResult <- lapply(lintFields, function(field) {
        result[[field]][[file]]
      })
      names(subResult) <- lintFields
      subResult$suggestion <- result$suggestion
      subResult$file <- file
      class(subResult) <- "lint"
      subResult
    })
    class(result) <- "lintList"
    result
  })
  names(fileResults) <- lintedFiles
  class(fileResults) <- "linterResults"
  fileResults
}

lintHeader <- function(file) {
  dashSep <- paste(rep("-", nchar(file)), collapse = "")
  header <- paste0(
    dashSep, "\n",
    file, "\n",
    dashSep, "\n"
  )
  cat(paste(header, collapse = "\n"))
}

#' @export
print.linterResults <- function(x, ...) {
  if (!hasLint(x)) {
    cat("No problems found\n")
    return(invisible(x))
  }

  lapply(x, printLintList, ...)

  cat(paste(collectSuggestions(x), collapse = "\n"))

  invisible(x)
}

printLintList <- function(x, ...) {
  has_message <- vapply(x, function(x) length(x$message), integer(1))
  if (!any(has_message)) {
    return()
  }
  lintHeader(x[[1]]$file)
  lapply(x, function(x) {
    cat(paste(x$message, collapse = "\n"))
  })
}

collectSuggestions <- function(fileResults) {
  suggestions <- lapply(fileResults, function(fileResult) {
    unlist(lapply(fileResult, function(lintInfo) {
      if (length(lintInfo$indices) > 0) {
        paste(as.character(lintInfo$suggestion), collapse = "\n")
      }
    }))
  })
  Reduce(union, suggestions)
}

showLintResults <- function(appDir, lintResults) {
  if (!hasLint(lintResults)) {
    return(invisible())
  }

  if (interactive()) {
    # if enabled, show warnings in friendly marker tab in addition to
    # printing to console
    if (getOption("rsconnect.rstudio_source_markers", TRUE) &&
        rstudioapi::hasFun("sourceMarkers"))
    {
      showRstudioSourceMarkers(appDir, lintResults)
    }
  }

  message("The following potential problems were identified in the project files:\n")
  print(lintResults)

  if (interactive()) {
    response <- readline("Do you want to proceed with deployment? [y/N]: ")
    if (tolower(substring(response, 1, 1)) != "y") {
      cli::cli_abort("Cancelling deployment.")
    }
  } else {
    # message(
    #   "\nIf you believe these errors are spurious, run:\n\n",
    #   "\tdeployApp(lint = FALSE)\n\n",
    #   "to disable linting."
    # )
    message("If your code fails to run post-deployment, ",
            "please double-check these messages.")

    invisible()
  }
}

#' Construct a Linter Message
#'
#' Pretty-prints a linter message. Primarily used as a helper
#' for constructing linter messages with [linter()].
#'
#' @param header A header message describing the linter.
#' @param content The content of the file that was linted.
#' @param lines The line numbers from `content` that contain lint.
makeLinterMessage <- function(header, content, lines) {

  lint <- attr(lines, "lint")

  c(
    paste0(header, ":"),
    paste(lines, ": ",
          content[lines],
          if (!is.null(lint)) paste("    ", lint, sep = ""),
          sep = ""),
    "\n"
  )
}

enumerate <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  result <- vector("list", length(X))
  for (i in seq_along(X)) {
    result[[i]] <- FUN(X[[i]], i, ...)
  }
  names(result) <- names(X)
  result
}

hasLint <- function(x) {
  any(unlist(lapply(x, function(x) {
    lapply(x, function(x) {
      length(x$indices) > 0
    })
  })))
}

activeEncoding <- function(project = getwd()) {

  defaultEncoding <- getOption("encoding")
  if (identical(defaultEncoding, "native.enc"))
    defaultEncoding <- "unknown"

  # attempt to locate .Rproj file
  files <- list.files(project, full.names = TRUE)
  rprojFile <- grep("\\.Rproj$", files, value = TRUE)
  if (length(rprojFile) != 1)
    return(defaultEncoding)

  # read the file
  contents <- readLines(rprojFile, warn = FALSE, encoding = "UTF-8")
  encodingLine <- grep("^Encoding:", contents, value = TRUE)
  if (length(encodingLine) != 1)
    return(defaultEncoding)

  # remove "Encoding:" prefix
  sub("^Encoding:\\s*", "", encodingLine)

}
