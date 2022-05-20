resolveApplication <- function(accountDetails, appName) {
  client <- clientForAccount(accountDetails)
  apps <- client$listApplications(accountDetails$accountId)
  for (app in apps) {
    if (identical(app$name, appName)) {
      return(app)
    }
  }

  stopWithApplicationNotFound(appName)
}

stopWithApplicationNotFound <- function(appName) {
  stop(paste("No application named '", appName, "' is currently deployed",
    sep = ""
  ), call. = FALSE)
}
