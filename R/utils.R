
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

readPassword <- function(prompt) {
  # user super secret function if using RStudio
  if (exists(".rs.askForPassword")) {
    password <- .rs.askForPassword(prompt)
  } else {

    os <- Sys.info()[['sysname']]

    echoOff <- function() {
      if (identical(os, "Darwin") || identical(os, "Linux")) {
        #system("stty cbreak -echo <&2")
      } else {
        # TODO: disable echo on Windows
      }
    }

    echoOn <- function() {
      if (identical(os, "Darwin") || identical(os, "Linux")) {
        #system("stty echo")
      } else {
        # TODO: enable echo on Windows
      }
    }
  
    echoOff()
    password <- readline(prompt)
    echoOn()
  }
  return (password)
}

# wrapper around read.dcf to workaround LC_CTYPE bug
readDcf <- function(...) {
  loc <- Sys.getlocale('LC_CTYPE')
  on.exit(Sys.setlocale('LC_CTYPE', loc))
  read.dcf(...)
}

#' @export
hr <- function(message = "", n = 80) {
  if (nzchar(message)) {
    r <- as.integer((n - nchar(message) - 2) / 2)
    hr <- paste(rep_len("#", r), collapse = '')
    cat(hr, message, hr, sep=" ", '\n')
  } else {
    hr <- paste(rep_len("#", n), collapse = '')
    cat(hr, sep="", '\n')
  }
}
