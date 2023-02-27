detectLocale <- function() {
  if (!isWindows()) {
    locales <- strsplit(Sys.getlocale("LC_CTYPE"), ".", fixed = TRUE)[[1]]
    locales[[1]]
  } else {
    tryCatch(windowsLocale(), error = function(err) "en_US")
  }
}

# Possible values
# listed at https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-lcid
windowsLocale <- function() {
  key <- utils::readRegistry("Control Panel\\International", hive = "HCU")
  normalizeLocale(key$LocaleName)
}

# Remove script tag, if present
normalizeLocale <- function(x) {
  pieces <- strsplit(x, "-")[[1]]
  all_upper <- pieces[pieces == toupper(pieces)]

  if (length(all_upper) == 0) {
    pieces[[1]]
  } else {
    paste0(pieces[[1]], "_", all_upper[[1]])
  }
}
