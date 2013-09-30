

shinyappsConfigDir <- function() {
  applicationConfigDir("shinyapps")
}

applicationConfigDir <- function(appName) {
  
  # get the home directory from the operating system (in case
  # the user has redefined the meaning of ~) but fault back 
  # to ~ if there is no HOME variable defined
  homeDir <- Sys.getenv("HOME", unset="~")
  
  # determine application config dir (platform specific)
  sysName <- Sys.info()[['sysname']]
  if (identical(sysName, "Windows"))
    configDir <- Sys.getenv("APPDATA")
  else if (identical(sysName, "Darwin"))
    configDir <- file.path(homeDir, "Library/Application Support")
  else
    configDir <- Sys.getenv("XDG_CONFIG_HOME", file.path(homeDir, ".config"))
  
  # append the application name and normalize
  configDir <- file.path(configDir, "R", appName)
  configDir <- normalizePath(configDir)
  
  # ensure that it exists
  if (!file.exists(configDir))
    dir.create(configDir, recursive=TRUE)
  
  # return it
  configDir
}
