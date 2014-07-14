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
  
  # Read in project files
  projectFiles <- list.files(project, full.names = TRUE, recursive = TRUE, all.files = TRUE)
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
      header <- paste(dashSep, "\n",
                      names(lintMessages)[i], "\n",
                      dashSep, "\n\n", sep = "")
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
  invisible(fileResults)
}
