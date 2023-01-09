#' rsconnect Configuration Directory
#'
#' Forms the path to a location on disk where user-level configuration data for
#' the package is stored.
#'
#' @param subDir An optional subdirectory to be included as the last element of
#'   the path.
#'
#' @return The path to the configuration directory.
#'
#' @keywords internal
rsconnectConfigDir <- function(subDir = NULL) {

  # Compute the name of the configuration directory using the standard R method
  configDir <- applicationConfigDir()

  # If the configuration directory doesn't exist, see if there's an old one to
  # migrate
  if (!file_test("-d", configDir)) {

    # For historical reasons too painful to enumerate here, there are not one
    # but *two* old locations for configuration files to check. If we find
    # one, we need to move its contents to the new folder.
    oldConfigDir <- oldApplicationConfigDir("rsconnect")
    if (!file_test("-d", oldConfigDir)) {
      oldConfigDir <- oldApplicationConfigDir("connect")
    }

    # We have no configuration directory but we do have an old one; migrate it.
    if (file_test("-d", oldConfigDir)) {

      # Create the parent folder if necessary
      dir.create(dirname(configDir), recursive = TRUE, showWarnings = FALSE)

      # Migrate the old directory to the new one
      file.rename(oldConfigDir, configDir)
    }
  }

  # Form the target and append the optional subdirectory if given
  target <- configDir
  if (!is.null(subDir)) {
    target <- file.path(target, subDir)
  }

  # Create the path if it doesn't exist
  dir.create(target, recursive = TRUE, showWarnings = FALSE)

  # Return completed path
  target
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
  normalizePath(configDir, mustWork = FALSE)
}

#' Application Configuration Directory
#'
#' Returns the root path used to store per user configuration data. Does not
#' check old locations or create the path; use \code{rsconnectConfigDir} for
#' most cases.
#'
#' @return A string containing the path of the configuration folder.
#'
#' @keywords internal
applicationConfigDir <- function() {

  if (exists("R_user_dir", envir = asNamespace("tools"))) {
    # In newer versions of R (>=4.0), we can ask R itself where configuration should be stored.
    # Load from the namespace to avoid check warnings with old R.
    f <- get("R_user_dir", envir = asNamespace("tools"))
    f("rsconnect", "config")
  } else {
    # In older versions of R, use an implementation derived from R_user_dir
    home <- Sys.getenv("HOME", unset = normalizePath("~"))
    path <-
      if (nzchar(p <- Sys.getenv("R_USER_CONFIG_DIR")))
        p
      else if (nzchar(p <- Sys.getenv("XDG_CONFIG_HOME")))
        p
      else if (.Platform$OS.type == "windows")
        file.path(Sys.getenv("APPDATA"), "R", "config")
      else if (Sys.info()["sysname"] == "Darwin")
        file.path(home, "Library", "Preferences", "org.R-project.R")
      else
        file.path(home, ".config")

    file.path(path, "R", "rsconnect")
  }
}
