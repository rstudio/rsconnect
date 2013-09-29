

setAccountInfo <- function(name, key, secret) {
  
}

accounts <- function() {
  
}

removeAccount <- function() {
  
}

accountsConfigDir <- function() {
  configDir <- file.path(shinyappsConfigDir(), "accounts")
  if (!file.exists(configDir))
    dir.create(configDir, recursive=TRUE)
  configDir
}
