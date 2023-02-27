detectLocale <- function() {
  if (!isWindows()) {
    locales <- strsplit(Sys.getlocale("LC_CTYPE"), ".", fixed = TRUE)[[1]]
    locales[[1]]
  } else {
    locale <- HCU_registry_key("Control Panel\\International\\User Profile")$Languages[[1]]
    if (is.null(locale)) {
      # Try approach that works on Windows 7
      locales <- HCU_registry_key("Control Panel\\International")$LocaleName
      if (is.null(locale)) {
        # Otherwise fall back US English
        locale <- "en-US"
      }
    }
    gsub("-", "_", locale)
  }
}

HCU_registry_key <- function(key, default = NULL) {
  tryCatch(
    utils::readRegistry(key, hive = "HCU"),
    error = function(err) {
      default
    }
  )
}
