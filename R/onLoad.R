.onLoad <- function(libname, pkgname) {
  # HTTP requests can take longer on Windows, so set a larger timeout
  # by default (primarily used by the IDE when validating server URLs)
  if (is.null(getOption("rsconnect.http.timeout"))) {
    windows <- identical(.Platform$OS.type, "windows")
    options(rsconnect.http.timeout = ifelse(windows, 20, 10))
  }

  if (is.null(getOption("rsconnect.metadata.sync.hours"))) {
    options(rsconnect.metadata.sync.hours = 24)
  }
}
