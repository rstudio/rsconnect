.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("rsconnect.max.bundle.size"))) {
    options(rsconnect.max.bundle.size = 3145728000)
  }
  if (is.null(getOption("rsconnect.max.bundle.files"))) {
    options(rsconnect.max.bundle.files = 10000)
  }
}
