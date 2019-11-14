.globals <- new.env(parent = emptyenv())

logTimestamper <- function() {
  paste("[", as.character(Sys.time()), "]", sep = "")
}

timestampedLog <- function(...) {
  cat(paste(logTimestamper(), ..., "\n"))
}

# Returns a logging function when enabled, a noop function otherwise.
verboseLogger <- function(verbose) {
  if (verbose) {
    timestampedLog
  } else {
    function(...) {}
  }
}

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

# Replacement for tools::file_path_sans_ext to work around an issue where
# filenames like "foo..ext" are not returned as "foo.".
file_path_sans_ext <- function(x, compression = FALSE) {
  if (compression) {
    x <- sub("[.](gz|bz2|xz)$", "", x)
  }
  sub("(.+)\\.[[:alnum:]]+$", "\\1", x)
}

#' Generate a line with embedded message
#'
#' Generates a message, surrounded with `#`, that extends
#' up to length `n`.
#' @param message A string (single-line message).
#' @param n The total number length of the generated string --
#'   the message is padded with `#` up to length `n`.
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

  # remove 'Encoding:' prefix
  sub("^Encoding:\\s*", "", encodingLine)

}

# Returns the MD5 for path as a raw sequence of 16 hexadecimal pairs.
fileMD5 <- function(path) {
  # Use digest::digest to compute file MD5. FIPS mode disables openssl::md5. Workaround until we can
  # migrate away from MD5 for file content checks.
  #
  # See: https://github.com/rstudio/rsconnect/issues/363

  if (is.null(path)) {
    return(digest::digest("", algo = "md5", serialize = FALSE, raw = TRUE))
  }

  digest::digest(path, algo = "md5", file = TRUE, raw = TRUE)
}

# Returns the MD5 for path as a 32-character concatenated string of hexadecimal characters.
fileMD5.as.string <- function(path) {
  md5.as.string(fileMD5(path))
}

# Returns the input md5 as a 32-character concatenated string of hexadecimal characters.
md5.as.string <- function(md5) {
  paste(md5, collapse = "")
}
