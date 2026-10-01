# Client generics
#
# Use this file to define generics that will be implemented for multiple client
# classes (connectClient, shinyAppsClient, and connectCloudClient).
# ------------------------------------------------------------------------------

#' Upload an application bundle to the server
#'
#' Each method reads the identifier it needs from `application`, because the
#' backends differ: Connect uses the content guid, shinyapps.io uses the
#' application id, and Connect Cloud uses the upload URL on the pending revision.
#'
#' @param client A client object: `connectClient`, `shinyAppsClient`, or
#'   `connectCloudClient`.
#' @param application The content object from the create or find step. The
#'   method reads the backend's identifier from it.
#' @param bundlePath Path to the bundle archive to upload.
#'
#' @return The bundle, which has an `id`. Connect Cloud has no bundle id, so its
#'   method returns `NULL`.
#' @noRd
uploadBundle <- function(client, application, bundlePath) {
  UseMethod("uploadBundle")
}

#' Get the display name of the server a client talks to
#'
#' Use this name in messages to the user.
#'
#' @param client A client object.
#' @return A string, for example `"Posit Connect"`.
#' @noRd
serverDisplayName <- function(client) {
  UseMethod("serverDisplayName")
}

#' Can a deploy set environment variables?
#' @noRd
supportsEnvVars <- function(client) {
  UseMethod("supportsEnvVars")
}

#' Can the account list and update environment variables on all its content?
#'
#' This is different from `supportsEnvVars()`. It needs the `getEnvVars` and
#' `setEnvVars` API calls.
#' @noRd
supportsEnvVarManagement <- function(client) {
  UseMethod("supportsEnvVarManagement")
}

#' Can the server run Node.js content?
#'
#' A `TRUE` result does not check the server version.
#' @noRd
supportsNodejs <- function(client) {
  UseMethod("supportsNodejs")
}

#' Can the user add, remove, and list the users of an application?
#' @noRd
supportsUserManagement <- function(client) {
  UseMethod("supportsUserManagement")
}

#' Must each deploy upload a bundle?
#' @noRd
requiresUpload <- function(client) {
  UseMethod("requiresUpload")
}

#' Is Python detection on by default?
#'
#' The `rsconnect.python.enabled` option overrides this value.
#' @noRd
pythonEnabledByDefault <- function(client) {
  UseMethod("pythonEnabledByDefault")
}

#' Can a deploy set the visibility of an application?
#' @noRd
supportsVisibility <- function(client) {
  UseMethod("supportsVisibility")
}

#' Can `syncAppMetadata()` update deployment records from the server?
#' @noRd
supportsMetadataSync <- function(client) {
  UseMethod("supportsMetadataSync")
}

#' Must static R Markdown be deployed as `"rmd-shiny"`?
#' @noRd
staticRmdNeedsShiny <- function(client) {
  UseMethod("staticRmdNeedsShiny")
}

#' Must the content URL get UTM parameters before it opens in the browser?
#' @noRd
addsUtmParameters <- function(client) {
  UseMethod("addsUtmParameters")
}
