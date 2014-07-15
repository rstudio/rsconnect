.__LINTERS__. <- new.env(parent = emptyenv())

addLinter <- function(name, linter) {
  assign(name, linter, envir = .__LINTERS__.)
}

linter <- function(apply, takes, message) {
  result <- list(
    apply = apply,
    takes = takes,
    message = message
  )
  class(result) <- "linter"
  result
}

lint <- function(project) {
  
  # Perform actions within the project directory (so relative paths are easily used)
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(project)
  
  # Read in project files
  projectFiles <- list.files(full.names = TRUE, recursive = TRUE, all.files = TRUE)
  projectFiles <- gsub("^\\./", "", projectFiles)
  names(projectFiles) <- projectFiles
  linters <- mget(objects(.__LINTERS__.), envir = .__LINTERS__.)
  
  # Identify all files that will be read in by one or more linters
  projectFilesToLint <- Reduce(union, lapply(linters, function(x) {
    x$takes(projectFiles)
  }))
  
  # Read in the files
  # TODO: perform this task more lazily?
  projectContent <- suppressWarnings(lapply(projectFilesToLint, readLines))
  lintResults <- vector("list", length(linters))
  names(lintResults) <- names(linters)
  
  ## Apply each linter
  for (i in seq_along(linters)) {
    linter <- linters[[i]]
    applicableFiles <- linter$takes(projectFilesToLint)
    lintIndices <- vector("list", length(applicableFiles))
    names(lintIndices) <- applicableFiles
    for (j in seq_along(applicableFiles)) {
      file <- applicableFiles[[j]]
      lintIndices[[j]] <- linter$apply(
        projectContent[[file]],
        project = project,
        path = file
      )
    }
    lintMessages <- enumerate(lintIndices, function(x, i) {
      if (length(x)) {
        linter$message(projectContent[[names(lintIndices)[i]]], x)
      } else {
        character()
      }
    })
    lintResults[[i]] <- list(
      files = applicableFiles,
      indices = lintIndices,
      message = lintMessages
    )
  }
  lintedFiles <- Reduce(union, lapply(lintResults, function(x) {
    names(x$indices)
  }))
  lintFields <- names(lintResults[[1]])
  fileResults <- lapply(lintedFiles, function(file) {
    result <- lapply(lintResults, function(result) {
      result <- lapply(lintFields, function(field) {
        result[[field]][[file]]
      })
      names(result) <- lintFields
      names(result)[names(result) == "files"] <- "file"
      class(result) <- "lint"
      result
    })
    class(result) <- "lintList"
    result
  })
  names(fileResults) <- lintedFiles
  class(fileResults) <- "linterResults"
  invisible(fileResults)
}

print.linterResults <- function(x, ...) {
  lapply(x, print, ...)
  invisible(x)
}

print.lintList <- function(x, ...) {
  lapply(x, print, ...)
  invisible(x)
}

print.lint <- function(x, ...) {
  if (!length(x$message))
    return(invisible(NULL))
  
  dashSep <- paste(rep("-", nchar(x$file)), collapse = "")
  header <- paste(dashSep, "\n",
                  x$file, "\n",
                  dashSep, "\n\n", sep = "")
  message(paste(header, paste(x$message, collapse = "\n"), collapse = "\n"))
  invisible(x)
}
