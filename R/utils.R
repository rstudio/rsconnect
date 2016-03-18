.globals <- new.env(parent = emptyenv())

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
  return (getOption("rsconnect.http.trace", FALSE) ||
          getOption("rsconnect.http.verbose", FALSE))
}

readPassword <- function(prompt) {
  if (rstudioapi::hasFun("askForPassword")) {
    password <- rstudioapi::askForPassword(prompt)
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


#' Generate a line with embedded message
#'
#' Generates a message, surrounded with \code{#}, that extends
#' up to length \code{n}.
#' @param message A string (single-line message).
#' @param n The total number length of the generated string --
#'   the message is padded with \code{#} up to length \code{n}.
#' @noRd
hr <- function(message = "", n = 80) {
  if (nzchar(message)) {
    r <- as.integer((n - nchar(message) - 2) / 2)
    hr <- paste(rep.int("#", r), collapse = '')
    cat(hr, message, hr, sep=" ", '\n')
  } else {
    hr <- paste(rep.int("#", n), collapse = '')
    cat(hr, sep="", '\n')
  }
}

# this function was defined in the shiny package; in the unlikely event that
# shiny:::checkEncoding() is not available, use a simplified version here
checkEncoding2 <- function(file) {
  tryCatch(
    getFromNamespace('checkEncoding', 'shiny')(file),
    error = function(e) {
      if (.Platform$OS.type != 'windows') return('UTF-8')
      x <- readLines(file, encoding = 'UTF-8', warn = FALSE)
      isUTF8 <- !any(is.na(iconv(x, 'UTF-8')))
      if (isUTF8) 'UTF-8' else getOption('encoding')
    }
  )
}

# if shiny:::checkEncoding() gives UTF-8, use it, otherwise first consider
# the RStudio project encoding, and eventually getOption('encoding')
checkEncoding <- function(file) {
  enc1 <- .globals$encoding
  enc2 <- checkEncoding2(file)
  if (enc2 == 'UTF-8') return(enc2)
  if (length(enc1)) enc1 else enc2
}

# read the Encoding field from the *.Rproj file
rstudioEncoding <- function(dir) {
  proj <- list.files(dir, '[.]Rproj$', full.names = TRUE)
  if (length(proj) != 1L) return()  # there should be one and only one .Rproj
  enc <- drop(readDcf(proj, 'Encoding'))
  enc[!is.na(enc)]
}

# return the leaf from a path (e.g. /foo/abc/def -> def)
fileLeaf <- function(path) {
  components <- strsplit(path, "/")
  unlist(lapply(components, function(component) {
    component[length(component)]
  }))
}

# whether the given path points to an individual piece of content
isDocumentPath <- function(path) {
  ext <- tolower(tools::file_ext(path))
  !is.null(ext) && ext != ""
}

# given a path, return the directory under which rsconnect package state is
# stored
rsconnectRootPath <- function(appPath) {
  if (isDocumentPath(appPath))
    file.path(dirname(appPath), "rsconnect", "documents", basename(appPath))
  else
    file.path(appPath, "rsconnect")
}

dirExists <- function(x) {
  utils::file_test('-d', x)
}

capitalize <- function(x) {
  if (nchar(x) == 0)
    x
  else
    paste0(toupper(substr(x, 1, 1)), substring(x, 2))
}


