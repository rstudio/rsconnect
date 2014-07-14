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
  projectFiles <- list.files(project, full.names = TRUE, recursive = TRUE, all.files = TRUE)
  names(projectFiles) <- projectFiles
  projectContent <- suppressWarnings(lapply(projectFiles, readLines))
  linters <- mget(objects(.__LINTERS__.), envir = .__LINTERS__.)
  lintResults <- vector("list", length(linters))
  names(lintResults) <- names(linters)
  for (i in seq_along(linters)) {
    linter <- linters[[i]]
    applicableFiles <- linter$takes(projectFiles)
    lintIndices <- lapply(projectContent[applicableFiles], linter$apply)
    lintMessages <- enumerate(lintIndices, function(x, i) {
      if (length(x)) {
        linter$message(projectContent[[names(lintIndices)[i]]], x)
      } else {
        character()
      }
    })
    messages <- enumerate(lintMessages, function(x, i) {
      if (!length(x)) return(character())
      name <- names(lintMessages)[i]
      dashSep <- paste(rep("-", nchar(name)), collapse = "")
      header <- paste(names(lintMessages)[i], "\n", dashSep, "\n\n", sep = "")
      msg <- paste(header, paste(x, collapse = "\n"), sep = "")
      message(msg)
      return(msg)
    })
    lintResults[[i]] <- list(
      indices = lintIndices,
      messages = lintMessages
    )
  }
  lintedFiles <- Reduce(union, lapply(lintResults, function(x) {
    names(x$indices)
  }))
  lintFields <- names(lintResults[[1]])
  fileResults <- lapply(lintedFiles, function(file) {
    lapply(lintResults, function(result) {
      result <- lapply(lintFields, function(field) {
        result[[field]][[file]]
      })
      names(result) <- lintFields
      result
    })
  })
  names(fileResults) <- lintedFiles
  fileResults
}
