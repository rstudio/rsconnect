.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("rsconnect.max.bundle.size"))) {
    options(rsconnect.max.bundle.size = 1048576000)
  }
}
