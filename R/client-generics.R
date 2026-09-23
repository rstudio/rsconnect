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
