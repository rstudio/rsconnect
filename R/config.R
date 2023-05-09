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
  configDir <- applicationConfigDir()

  # If the configuration directory doesn't exist, see if there's an old one to
  # migrate
  if (!dirExists(configDir)) {
    migrateConfig(configDir)
  }

  # Form the target and append the optional subdirectory if given
  target <- configDir
  if (!is.null(subDir)) {
    target <- file.path(target, subDir)
  }

  dirCreate(target)
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

# server ------------------------------------------------------------------

serverConfigDir <- function() {
  rsconnectConfigDir("servers")
}

serverConfigFile <- function(name) {
  normalizePath(
    file.path(serverConfigDir(), paste(name, ".dcf", sep = ""))
  )
}

serverConfigFiles <- function() {
  list.files(serverConfigDir(), pattern = glob2rx("*.dcf"), full.names = TRUE)
}

# account -----------------------------------------------------------------

accountConfigDir <- function() {
  rsconnectConfigDir("accounts")
}

accountConfigFile <- function(name, server) {
  normalizePath(
    file.path(accountConfigDir(), server, paste(name, ".dcf", sep = ""))
  )
}

accountConfigFiles <- function(server = NULL) {
  path <- accountConfigDir()
  if (!is.null(server)) {
    path <- file.path(path, server)
  }

  list.files(path, pattern = glob2rx("*.dcf"), recursive = TRUE, full.names = TRUE)
}


# deployments -------------------------------------------------------------

deploymentHistoryPath <- function(new = FALSE) {
  file.path(
    rsconnectConfigDir("deployments"),
    paste0("history", if (new) ".new", ".dcf")
  )
}

# given a path, return the directory under which rsconnect package state is
# stored
deploymentConfigDir <- function(recordPath) {
  if (isDocumentPath(recordPath)) {
    file.path(dirname(recordPath), "rsconnect", "documents", basename(recordPath))
  } else {
    file.path(recordPath, "rsconnect")
  }
}

deploymentConfigFile <- function(recordPath, name, account, server) {
  accountDir <- file.path(deploymentConfigDir(recordPath), server, account)
  dirCreate(accountDir)
  file.path(accountDir, paste0(name, ".dcf"))
}

deploymentConfigFiles <- function(recordPath) {
  dir <- deploymentConfigDir(recordPath)
  list.files(dir, glob2rx("*.dcf"), recursive = TRUE, full.names = TRUE)
}

# Does the path point to an individual piece of content?
isDocumentPath <- function(path) {
  tools::file_ext(path) != ""
}
