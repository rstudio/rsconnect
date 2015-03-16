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
                                  getOption('shinyapps.locale.cache', TRUE)) {
  
  cacheFile <- localeCacheFile()
  if (file.exists(cacheFile) && useCache) {
    
    # get chached
    cache <- as.list(readDcf(cacheFile, all=TRUE))  
    
    locale <- unlist(cache$locale)
    
  } else {
    
    # get system locale
    locale <- systemLocale()
    
    # write the user info
    write.dcf(list(locale = locale),
              cacheFile,
              width = 100)
  }
  
  return(locale)
}

localeCacheFile <- function() {
  normalizePath(file.path(shinyappsConfigDir(), "locale.dcf"), mustWork = FALSE)
}

systemLocale <- function() {
  message("Detecting system locale ... ", appendLF = FALSE)
  
  # get system locale
  info <- systemInfo()
  raw <- as.character(info$System.Locale)
  parts <- strsplit(unlist(strsplit(raw, ";",  fixed=TRUE)), "-", fixed=TRUE)
  
  # normalize locale to something like en_US
  locale <- paste(tolower(parts[[1]][1]), toupper(parts[[1]][2]), sep="_")
  
  message(locale)
  invisible(locale)
}

systemInfo <- function () {
  info <- read.csv(textConnection(system("systeminfo /FO csv", intern=TRUE, wait=TRUE)))
}