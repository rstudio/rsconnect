overrideWindowsLocale <- function(locale) {
  map <- list()
  map[['el_EL']] <- "el_GR"
  if (locale %in% names(map)) {
    locale <- map[[locale]]
  }
  return(locale)
}

detectLocale <- function () {
  sysName <- Sys.info()[['sysname']]
  if (identical(sysName, "Windows")) {
    locale <- detectLocale.Windows()
  } else {
    locale <- detectLocale.Unix()
  }
  return(locale)
}

detectLocale.Unix <- function () {
  unlist(strsplit(Sys.getlocale("LC_CTYPE"), ".", fixed=TRUE))[1]
}

detectLocale.Windows <- function (useCache =
                                  getOption('rsconnect.locale.cache', TRUE)) {

  # default locale
  locale <- 'en_US'

  cacheFile <- localeCacheFile()
  if (file.exists(cacheFile) && useCache) {

    # get chached
    cache <- as.list(readDcf(cacheFile, all=TRUE))

    locale <- unlist(cache$locale)

  } else {

    tryCatch({

      # get system locale
      locale <- systemLocale()

      # write the user info
      write.dcf(list(locale = locale),
                cacheFile,
                width = 100)

    }, error=function(e) {
      warning(paste0("Error detecting locale: ", e,
                     " (Using default: ", locale, ")"), call.=FALSE)
    })
  }
  return(overrideWindowsLocale(locale))
}

localeCacheFile <- function() {
  normalizePath(file.path(rsconnectConfigDir(), "locale.dcf"), mustWork = FALSE)
}

systemLocale <- function() {
  message("Detecting system locale ... ", appendLF = FALSE)

  # get system locale
  info <- systemInfo()
  raw <- as.character(info[[20]])
  parts <- strsplit(unlist(strsplit(raw, ";",  fixed=TRUE)), "-", fixed=TRUE)

  if (length(parts[[1]]) >= 2) {
    # normalize locale to something like en_US
    locale <- paste(tolower(parts[[1]][1]), toupper(parts[[1]][2]), sep="_")
  } else {
    locale <- paste(tolower(parts[[1]][1]), toupper(parts[[1]][1]), sep="_")
  }
  message(locale)
  return(locale)
}

systemInfo <- function () {
  raw <- system("systeminfo /FO csv", intern=TRUE, wait=TRUE)
  info <- read.csv(textConnection(raw))
  return(info)
}
