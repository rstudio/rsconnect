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
  } else {
    return(NULL)
  }
}

displayStatus <- function(quiet) {
  quiet <- quiet || httpDiagnosticsEnabled()
  function(status) {
    if (!quiet) {
      cat(status)
    }
  }
}

httpDiagnosticsEnabled <- function() {
  return(getOption("rsconnect.http.trace", FALSE) ||
    getOption("rsconnect.http.verbose", FALSE))
}

# Replacement for tools::file_path_sans_ext to work around an issue where
# filenames like "foo..ext" are not returned as "foo.".
file_path_sans_ext <- function(x, compression = FALSE) {
  if (compression) {
    x <- sub("[.](gz|bz2|xz)$", "", x)
  }
  sub("(.+)\\.[[:alnum:]]+$", "\\1", x)
}

dirExists <- function(x) {
  utils::file_test("-d", x)
}

# Returns the MD5 for path as a raw sequence of 16 hexadecimal pairs.
fileMD5 <- function(path, raw = FALSE) {
  # Use digest::digest to compute file MD5. FIPS mode disables openssl::md5. Workaround until we can
  # migrate away from MD5 for file content checks.
  #
  # See: https://github.com/rstudio/rsconnect/issues/363

  if (is.null(path)) {
    digest::digest("", algo = "md5", serialize = FALSE, raw = raw)
  } else {
    digest::digest(path, algo = "md5", file = TRUE, raw = raw)
  }
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
    df[setdiff(all_names, names(df))] <- rep(NA, nrow(df))
    df
  }

  complete <- lapply(dfs, add_missing_cols)
  out <- do.call("rbind", complete)
  out[all_names]
}

# Ensure slashes are the same direction on every platform to make snapshot
# testing simpler
normalizePath <- function(path, mustWork = FALSE) {
  base::normalizePath(path, winslash = "/", mustWork = mustWork)
}

compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

hasPrefix <- function(x, prefix) {
  substring(x, 1, nchar(prefix)) == prefix
}

# Lightweight equivalent of withr::defer()
defer <- function(expr, env = caller_env(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = env)
}

dirCreate <- function(paths) {
  for (path in paths) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
  paths
}

fromIDE <- function() {
  !is.na(Sys.getenv("RSTUDIO", unset = NA)) && !identical(.Platform$GUI, "RStudio")
}

toJSON <- function(x, ...) {
  jsonlite::toJSON(
    x,
    dataframe = "columns",
    null = "null",
    na = "null",
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = 30,
    ...
  )
}

truthy <- function(value, default = FALSE) {
  if (!is.atomic(value) || length(value) != 1 || is.na(value))
    default
  else if (is.character(value))
    value %in% c("TRUE", "True", "true", "T", "1")
  else
    as.logical(value)
}
