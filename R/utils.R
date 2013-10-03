
isStringParam <- function(param) {
  is.character(param) && (length(param) == 1)
}

stringParamErrorMessage <- function(param) {
  paste(param, "must be a single element character vector")
}

regexExtract <- function(re, input) {
  match <- regexec(re, input)
  matchLoc <- match[1][[1]]
  if (length(matchLoc) > 1) {
    matchLen <-attributes(matchLoc)$match.length
    return (substr(input, matchLoc[2], matchLoc[2] + matchLen[2]-1))
  }
  else {
    return (NULL)
  }
}

displayStatus <- function(quiet) {
  quiet <- quiet || httpDiagnosticsEnabled()
  function (status) {
    if (!quiet)
      cat(status)
  }
}

withStatus <- function(quiet) {
  quiet <- quiet || httpDiagnosticsEnabled()
  function(status, code) {
    if (!quiet)
      cat(status, "...", sep="")
    force(code)
    if (!quiet)
      cat("DONE\n")
  }
}

httpDiagnosticsEnabled <- function() {
  return (getOption("shinyapps.http.trace", FALSE) ||
          getOption("shinyapps.http.verbose", FALSE))
}


