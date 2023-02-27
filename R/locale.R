detectLocale <- function() {
  if (!isWindows()) {
    locales <- strsplit(Sys.getlocale("LC_CTYPE"), ".", fixed = TRUE)[[1]]
    locales[[1]]
  } else {
    # Possible values listed at https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-lcid/70feba9f-294e-491e-b6eb-56532684c37f
    locales <- HCU_registry_key("Control Panel\\International")$LocaleName

    if (is.null(locale)) {
      # Otherwise fall back US English
      locale <- "en-US"
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
