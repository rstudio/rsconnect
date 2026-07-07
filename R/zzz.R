defaultMaxBundleSize <- 5 * 1024^3
defaultMaxBundleFiles <- 10000

setOptionDefaults <- function(...) {
  # Resolve dots
  defaultOptions <- list(...)

  # Get current options
  currentOptions <- options()

  # Remove any options which have already been set
  alreadySet <- names(defaultOptions) %in% names(currentOptions)
  defaultOptions <- defaultOptions[!alreadySet]

  # Set the defaults
  options(defaultOptions)
}

.onLoad <- function(libname, pkgname) {
  setOptionDefaults(
    rsconnect.max.bundle.size = defaultMaxBundleSize,
    rsconnect.max.bundle.files = defaultMaxBundleFiles,
    rsconnect.check_updates = TRUE,
    rsconnect.update_check_timeout = 5
  )
}

.onAttach <- function(libname, pkgname) {
  if (!is_interactive()) {
    return(invisible())
  }

  newer <- tryCatch(checkForNewerVersion(pkgname), error = function(e) NULL)
  if (!is.null(newer)) {
    packageStartupMessage(
      updateStartupMessage(utils::packageVersion(pkgname), newer)
    )
  }
}
