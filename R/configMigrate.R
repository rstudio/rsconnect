# account/server ----------------------------------------------------------

migrateConfig <- function(configDir) {
  # For historical reasons too painful to enumerate here, there are not one
  # but *two* old locations for configuration files to check. If we find
  # one, we need to move its contents to the new folder.
  oldConfigDir <- oldApplicationConfigDir("rsconnect")
  if (!dirExists(oldConfigDir)) {
    oldConfigDir <- oldApplicationConfigDir("connect")
  }

  # We have no configuration directory but we do have an old one; migrate it.
  if (dirExists(oldConfigDir)) {

    # Create the parent folder if necessary
    dirCreate(dirname(configDir))

    # Migrate the old directory to the new one
    file.rename(oldConfigDir, configDir)
  }
}

#' Old Application Config Directory
#'
#' Returns the old application configuration directory used by rsconnect
#' 0.8.24 and prior. These versions wrote configuration data to XDG compliant
#' locations, but CRAN policy has since further restricted the disk locations
#' that are permitted. See:
#'
#' https://cran.r-project.org/web/packages/policies.html
#'
#' @param appName The application's name (connect or rsconnect)
#'
#' @return The old application configuration directory.
#'
#' @keywords internal
oldApplicationConfigDir <- function(appName) {

  # get the home directory from the operating system (in case
  # the user has redefined the meaning of ~) but fault back
  # to ~ if there is no HOME variable defined
  homeDir <- Sys.getenv("HOME", unset = "~")

  # check for R specific config dir
  configDir <- Sys.getenv("R_USER_CONFIG_DIR")

  if (nzchar(configDir)) {
    # R specific config dir, append app name only
    configDir <- file.path(configDir, appName)
  } else {
    # no R specific config dir; determine application config dir (platform specific)
    sysName <- Sys.info()[["sysname"]]
    if (identical(sysName, "Windows"))
      configDir <- Sys.getenv("APPDATA")
    else if (identical(sysName, "Darwin"))
      configDir <- file.path(homeDir, "Library/Application Support")
    else
      configDir <- Sys.getenv("XDG_CONFIG_HOME", file.path(homeDir, ".config"))

    # append the application name
    configDir <- file.path(configDir, "R", appName)
  }

  # normalize path
  normalizePath(configDir)
}

# deployments -------------------------------------------------------------

migrateDeploymentsConfig <- function(appPath) {
  # calculate migration dir--all shinyapps deployment records go into the root
  # folder since it wasn't possible to deploy individual docs using the
  # shinyapps package
  migrateRoot <- if (isDocumentPath(appPath)) dirname(appPath) else appPath

  # migrate shinyapps package created records if necessary
  shinyappsDir <- file.path(migrateRoot, "shinyapps")
  if (!file.exists(shinyappsDir)) {
    return()
  }

  migrateDir <- file.path(migrateRoot, "rsconnect")
  for (shinyappsFile in list.files(shinyappsDir, glob2rx("*.dcf"),
                                   recursive = TRUE)) {
    # read deployment record
    shinyappsDCF <- file.path(shinyappsDir, shinyappsFile)
    deployment <- as.data.frame(read.dcf(shinyappsDCF),
                                stringsAsFactors = FALSE)
    deployment$server <- "shinyapps.io"

    # write the new record
    rsconnectDCF <- file.path(migrateDir, "shinyapps.io", shinyappsFile)
    dirCreate(dirname(rsconnectDCF))
    write.dcf(deployment, rsconnectDCF)

    # remove old DCF
    file.remove(shinyappsDCF)
  }

  # remove shinyapps dir if it's completely empty
  remainingFiles <- list.files(shinyappsDir,
                               recursive = TRUE,
                               all.files = TRUE)
  if (length(remainingFiles) == 0)
    unlink(shinyappsDir, recursive = TRUE)
}
