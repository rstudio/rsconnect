

rsconnectConfigDir <- function(subDir = NULL) {

  # first check whether the main rsconnect directory exists
  config_dir <- applicationConfigDir("rsconnect", create = FALSE)

  # if it doesn't exist then see whether there is a main "connect" directory to migrate
  if (!file_test("-d", config_dir)) {
    old_config_dir <- applicationConfigDir("connect", create = FALSE)
    if (file_test("-d", old_config_dir))
      if (!file.rename(old_config_dir, config_dir))
        return(old_config_dir)
  }

  # return the directory
  applicationConfigDir("rsconnect", subDir)
}

applicationConfigDir <- function(appName, subDir = NULL, create = TRUE) {

  # get the home directory from the operating system (in case
  # the user has redefined the meaning of ~) but fault back
  # to ~ if there is no HOME variable defined
  homeDir <- Sys.getenv("HOME", unset="~")

  # check for R specific config dir
  configDir <- Sys.getenv("R_USER_CONFIG_DIR")

  if (nzchar(configDir)) {
    # R specific config dir, append app name only
    configDir <- file.path(configDir, appName)
  } else {
    # no R specific config dir; determine application config dir (platform specific)
    sysName <- Sys.info()[['sysname']]
    if (identical(sysName, "Windows"))
      configDir <- Sys.getenv("APPDATA")
    else if (identical(sysName, "Darwin"))
      configDir <- file.path(homeDir, "Library/Application Support")
    else
      configDir <- Sys.getenv("XDG_CONFIG_HOME", file.path(homeDir, ".config"))

    # append the application name
    configDir <- file.path(configDir, "R", appName)
  }

  # append optional subdirectory
  if (!is.null(subDir))
    configDir <- file.path(configDir, subDir)

  # normalize path
  configDir <- normalizePath(configDir, mustWork=FALSE)

  # ensure that it exists
  if (!file.exists(configDir) && create)
    dir.create(configDir, recursive=TRUE)

  # return it
  configDir
}
