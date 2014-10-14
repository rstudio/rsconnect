.__LINTERS__. <- new.env(parent = emptyenv())

##' Add a Linter
##' 
##' Add a linter, to be used in subsequent calls to \code{\link{lint}}.
##' 
##' @param name The name of the linter, as a string.
##' @param linter A \code{\link{linter}}.
##' @export
##' @example examples/example-linter.R
addLinter <- function(name, linter) {
  assign(name, linter, envir = .__LINTERS__.)
}


##' Create a Linter
##' 
##' Generate a linter, which can identify errors or problematic regions in a 
##' project.
##' 
##' @param apply Function that, given the content of a file, returns the indices
##'   at which problems were found.
##' @param takes Function that, given a set of paths, returns the subset of 
##'   paths that this linter uses.
##' @param message Function that, given content and lines, returns an 
##'   informative message for the user. Typically generated with 
##'   \code{\link{makeLinterMessage}}.
##' @param suggestion String giving a prescribed fix for the linted problem.
##' @export
##' @example examples/example-linter.R
linter <- function(apply, takes, message, suggestion) {
  result <- list(
    apply = apply,
    takes = takes,
    message = message,
    suggestion = suggestion
  )
  class(result) <- "linter"
  result
}

getLinterApplicableFiles <- function(linter, files) {
  result <- linter$takes(files)
  if (is.numeric(result) || is.logical(result)) {
    files[result]
  } else {
    result
  }
}

applyLinter <- function(linter, ...) {
  result <- linter$apply(...)
  if (is.logical(result)) {
    return(which(result))
  } else {
    return(as.numeric(result))
  }
}

##' Lint a Project
##' 
##' Takes the set of active linters (see \code{\link{addLinter}}), and applies
##' them to all files within a project.
##' 
##' @param project Path to a project directory.
##' @export
lint <- function(project) {
  
  if (!file.exists(project))
    stop("No directory at path '", project, "'")
  
  if (file.exists(project) && !isTRUE(file.info(project)$isdir))
    stop("Path '", project, "' is not a directory")
  
  # Perform actions within the project directory (so relative paths are easily used)
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(project)
  
  # List all files within the project
  projectFiles <- list.files(full.names = TRUE, recursive = TRUE, all.files = TRUE)
  projectFiles <- gsub("^\\./", "", projectFiles)
  names(projectFiles) <- projectFiles
  
  # Do some checks for a valid application structure
  appFilesBase <- tolower(list.files())
  wwwFiles <- tolower(list.files("www/"))
  satisfiedLayouts <- c(
    shinyAndUi = all(c("server.r", "ui.r") %in% appFilesBase),
    shinyAndIndex = "server.r" %in% appFilesBase && "index.html" %in% wwwFiles,
    app = "app.r" %in% appFilesBase,
    Rmd = any(grepl(glob2rx("*.rmd"), appFilesBase))
  )
  
  if (!any(satisfiedLayouts)) {
    msg <- "Cancelling deployment: invalid project layout.
            The project should have one of the following layouts:
            1. 'shiny.R' and 'ui.R' in the application base directory,
            2. 'shiny.R' and 'www/index.html' in the application base directory,
            3. An R Markdown (.Rmd) document."
    
    # strip leading whitespace from the above
    msg <- paste(collapse = "\n",
                 gsub("^ *", "", unlist(strsplit(msg, "\n", fixed = TRUE))))
                 
    stop(msg)
  }
  
  linters <- mget(objects(.__LINTERS__.), envir = .__LINTERS__.)
  
  # Identify all files that will be read in by one or more linters
  projectFilesToLint <- Reduce(union, lapply(linters, function(linter) {
    getLinterApplicableFiles(linter, projectFiles)
  }))
  
  # Read in the files
  # TODO: perform this task more lazily?
  projectContent <- suppressWarnings(lapply(projectFilesToLint, readLines))
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
      lintIndices[[j]] <- applyLinter(linter,
                                      projectContent[[file]],
                                      project = project,
                                      path = file,
                                      files = projectFiles)
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
  invisible(fileResults)
}

printLintHeader <- function(x) {
  if (!length(x$message)) return(invisible(NULL))
  dashSep <- paste(rep("-", nchar(x$file)), collapse = "")
  header <- paste(dashSep, "\n",
                  x$file, "\n",
                  dashSep, "\n", sep = "")
  message(paste(header, collapse = "\n"), appendLF = FALSE)
  invisible(x)
}

printLintBody <- function(x, ...) {
  message(paste(x$message, collapse = "\n"), appendLF = FALSE)
  invisible(x)
}

printLintFooter <- function(x, ...) {
  message(paste(collectSuggestions(x), collapse = "\n"))
  invisible(x)
}

printLinterResults <- function(x, ...) {
  lapply(x, printLintList, ...)
  printLintFooter(x)
  invisible(x)
}

printLintList <- function(x, ...) {
  printLintHeader(x[[1]])
  lapply(x, printLintBody, ...)
  invisible(x)
}

printLint <- function(x, ...) {
  printLintHeader(x)
  printLintBody(x, ...)
  invisible(x)
}

collectSuggestions <- function(fileResults) {
  suggestions <- lapply(fileResults, function(fileResult) {
    unlist(lapply(fileResult, function(lintInfo) {
      if (length(lintInfo$indices) > 0) {
        paste(as.character(lintInfo$suggestion), collapse = "\n")
      }
    }))
  })
  Reduce(intersect, suggestions)
}
