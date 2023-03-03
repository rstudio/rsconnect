# Returns a logging function when enabled, a noop function otherwise.
verboseLogger <- function(verbose) {
  if (verbose) {
    function(...) {
      timestamp <- paste("[", as.character(Sys.time()), "]", sep = "")
      cat(paste0(timestamp, " ", ..., "\n"))
    }
  } else {
    function(...) {}
  }
}

regexExtract <- function(re, input) {
  match <- regexec(re, input)
  matchLoc <- match[1][[1]]
  if (length(matchLoc) > 1) {
    matchLen <- attributes(matchLoc)$match.length
    return(substr(input, matchLoc[2], matchLoc[2] + matchLen[2] - 1))
  }
  else {
    return(NULL)
  }
}

displayStatus <- function(quiet) {
  quiet <- quiet || httpDiagnosticsEnabled()
  function(status) {
    if (!quiet)
      cat(status)
  }
}

httpDiagnosticsEnabled <- function() {
  return(getOption("rsconnect.http.trace", FALSE) ||
         getOption("rsconnect.http.verbose", FALSE))
}

readPassword <- function(prompt) {
  if (rstudioapi::hasFun("askForPassword")) {
    password <- rstudioapi::askForPassword(prompt)
  } else {

    os <- Sys.info()[["sysname"]]

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
  return(password)
}

# wrapper around read.dcf to workaround LC_CTYPE bug
readDcf <- function(...) {
  loc <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", loc))
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
    hr <- paste(rep.int("#", r), collapse = "")
    cat(hr, message, hr, sep = " ", "\n")
  } else {
    hr <- paste(rep.int("#", n), collapse = "")
    cat(hr, sep = "", "\n")
  }
}

dirExists <- function(x) {
  utils::file_test("-d", x)
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

  # remove "Encoding:" prefix
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

# Escape characters that have special meaning for extended regular expressions as supported by
# list.files.
#
# Similar to rex::escape
escapeRegex <- function(name) {
  gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", name, perl = TRUE)
}

check_file <- function(x,
                            error_arg = caller_arg(x),
                            error_call = caller_env()) {

  check_string(
    x,
    allow_empty = FALSE,
    error_arg = error_arg,
    error_call = error_call
  )
  if (!file.exists(x)) {
    cli::cli_abort(
      "{.arg {error_arg}}, {.str {x}}, does not exist.",
      call = error_call
    )
  }
}

check_directory <- function(x,
                            error_arg = caller_arg(x),
                            error_call = caller_env()) {

  check_file(x, error_arg = error_arg, error_call = error_call)
  if (!dirExists(x)) {
    cli::cli_abort(
      "{.arg {error_arg}}, {.str {x}}, is not a directory.",
      call = error_call
    )
  }
}

file_size <- function(path) {
  x <- file.info(path)$size
  x[is.na(x)] <- 0
  x
}

rbind_fill <- function(dfs, col_names = character()) {
  if (length(dfs) == 0) {
    df <- rep(list(logical(0)), length(col_names))
    names(df) <- col_names
    return(as.data.frame(df))
  }


  all_names <- unique(unlist(lapply(dfs, names)))
  all_names <- union(col_names, all_names)

  add_missing_cols <- function(df) {
    df[setdiff(all_names, names(df))] <- NA
    df
  }

  complete <- lapply(dfs, add_missing_cols)
  out <- do.call("rbind", complete)
  out[all_names]
}

normalizePath <- function(path, mustWork = FALSE) {
  base::normalizePath(path, winslash = "/", mustWork = mustWork)
}
