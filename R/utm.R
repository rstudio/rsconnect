getUtmValue <- function() {
  if (Sys.getenv("RSTUDIO") == "1") {
    return("rsconnect-rstudio")
  } else {
    return("rsconnect")
  }
}

# Add UTM query parameter for Posit Connect Cloud.
addUtmParameters <- function(url) {
  utmValue <- getUtmValue()
  if (grepl("\\?", url)) {
    paste0(url, "&utm_source=", utmValue)
  } else {
    paste0(url, "?utm_source=", utmValue)
  }
}
