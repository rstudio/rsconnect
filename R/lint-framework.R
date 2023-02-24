.__LINTERS__. <- new.env(parent = emptyenv())

##' Add a Linter
##'
##' Add a linter, to be used in subsequent calls to [lint()].
##'
##' @param name The name of the linter, as a string.
##' @param linter A [linter()].
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
##'   [makeLinterMessage()].
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
    output <- which(result)
  } else {
    output <- as.numeric(result)
  }
  attributes(output) <- attributes(result)
  output
}
