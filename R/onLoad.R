.onLoad <- function(libname, pkgname) {

  if (is.null(getOption("rsconnect.max.bundle.size"))) {
    options(rsconnect.max.bundle.size = 3145728000)
  }

  if (is.null(getOption("rsconnect.max.bundle.files"))) {
    options(rsconnect.max.bundle.files = 10000)
  }

  # HTTP requests can take longer on Windows, so set a larger timeout
  # by default (primarily used by the IDE when validating server URLs)
  if (is.null(getOption("rsconnect.http.timeout"))) {
    windows <- identical(.Platform$OS.type, "windows")
    options(rsconnect.http.timeout = ifelse(windows, 20, 5))
  }

}
