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

#' Create new content on the server
#'
#' @param client A client object.
#' @param deployment The deployment from `findDeploymentTarget()`.
#' @param accountDetails The account that deploys the content.
#' @param appMetadata The result of `appMetadata()`.
#' @param appVisibility The requested visibility, or `NULL`.
#'
#' @return The new content, which has an `id`. It has a `url` if the server
#'   knows the URL before the deploy. If not, `activateContent()` gets the URL.
#' @noRd
createContent <- function(
  client,
  deployment,
  accountDetails,
  appMetadata,
  appVisibility = NULL
) {
  UseMethod("createContent")
}

#' Get the content for an existing deployment record
#'
#' @param client A client object.
#' @param deployment The deployment from `findDeploymentTarget()`. It has an
#'   `appId`.
#' @param quiet If `TRUE`, do not show messages.
#'
#' @return The content. If the content does not exist, the method signals an
#'   `rsconnect_http_404` error.
#' @noRd
findContent <- function(client, deployment, quiet) {
  UseMethod("findContent")
}

#' Prepare content before the upload
#'
#' Apply the settings that must be on the content before it deploys, for
#' example visibility and environment variables.
#'
#' @param client A client object.
#' @param application The content from `findContent()` or `createContent()`.
#' @param deployment The deployment from `findDeploymentTarget()`.
#' @param appMetadata The result of `appMetadata()`.
#' @param appVisibility The requested visibility, or `NULL`.
#' @param isNewContent `TRUE` if `createContent()` made the content during this
#'   deploy.
#' @param upload `TRUE` if the deploy uploads a new bundle.
#' @param quiet If `TRUE`, do not show messages.
#'
#' @return The content. Use this value in the next steps, because a method can
#'   replace the content object.
#' @noRd
prepareContent <- function(
  client,
  application,
  deployment,
  appMetadata,
  appVisibility,
  isNewContent,
  upload,
  quiet
) {
  UseMethod("prepareContent")
}

#' Deploy the bundle and wait until the deploy completes
#'
#' @param client A client object.
#' @param application The content from `prepareContent()`.
#' @param bundle The bundle from `uploadBundle()`, or `NULL` when the deploy
#'   did not upload.
#' @param quiet If `TRUE`, do not show messages.
#'
#' @return A list with three fields. `succeeded` is `TRUE` or `FALSE`. `url` is
#'   the content URL. `error` is the error message, or `NULL`.
#' @noRd
activateContent <- function(client, application, bundle, quiet) {
  UseMethod("activateContent")
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

#' Can an application directory have a legacy scrypt password file?
#' @noRd
usesPasswordFile <- function(client) {
  UseMethod("usesPasswordFile")
}

#' Can the caller choose not to send the invitation email?
#' @noRd
supportsOptionalInviteEmail <- function(client) {
  UseMethod("supportsOptionalInviteEmail")
}

#' Can the server redact user emails in the list of users?
#'
#' A redacted email does not match the email that the caller gives.
#' @noRd
redactsUserEmails <- function(client) {
  UseMethod("redactsUserEmails")
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

#' Check that the server accepts an `appVisibility` value
#'
#' @param client A client object.
#' @param appVisibility The visibility value. It is not `NULL`.
#' @param error_call The call to show in the error.
#'
#' @return `NULL`, invisibly. The method signals an error if the server does
#'   not accept the value.
#' @noRd
validateVisibility <- function(
  client,
  appVisibility,
  error_call = caller_env()
) {
  UseMethod("validateVisibility")
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

#' Find the content that a collaborator function acts on
#'
#' @param client A client object.
#' @param accountDetails The account from `accountInfo()`.
#' @param appDir The directory that contains the deployment record.
#' @param appName The name of the application, or `NULL`.
#' @param contentId The content id, or `NULL`. Only Connect Cloud supports it.
#'
#' @return A list with two fields. `id` is the content id. `deploymentFile` is
#'   the path of the deployment record, or `NULL` if no record was used.
#' @noRd
resolveContentTarget <- function(
  client,
  accountDetails,
  appDir,
  appName,
  contentId = NULL
) {
  UseMethod("resolveContentTarget")
}

#' List the users who can access an application
#'
#' @param client A client object.
#' @param applicationId The content id from `resolveContentTarget()`.
#'
#' @return A data frame with one row for each user. It always has the columns
#'   `id`, `email`, and `account`. A method can add more columns.
#' @noRd
listCollaborators <- function(client, applicationId) {
  UseMethod("listCollaborators")
}

#' List the invitations that are not accepted for an application
#'
#' @param client A client object.
#' @param applicationId The content id from `resolveContentTarget()`.
#'
#' @return A data frame with one row for each invitation, and the columns
#'   `id`, `email`, `link`, and `expired`.
#' @noRd
listInvitations <- function(client, applicationId) {
  UseMethod("listInvitations")
}

#' Build the data frame that `applications()` returns
#'
#' @param client A client object.
#' @param accountDetails The account from `accountInfo()`.
#'
#' @return A data frame with one row for each content item. It has the columns
#'   `id`, `name`, `title`, `url`, `status`, `size`, `instances`, `config_url`,
#'   `created_time`, `updated_time`, and `guid`. A backend that does not have a
#'   value for a column puts `NA` in it.
#' @noRd
applicationsTable <- function(client, accountDetails) {
  UseMethod("applicationsTable")
}

#' Find existing content by name, for a deploy that has no deployment record
#'
#' @param client A client object.
#' @param accountDetails The account from `accountInfo()`.
#' @param name The name of the application.
#'
#' @return The content, or `NULL` if the server has no content with that name.
#' @noRd
findContentByName <- function(client, accountDetails, name) {
  UseMethod("findContentByName")
}

#' @export
findContentByName.rsconnectClient <- function(client, accountDetails, name) {
  tryCatch(
    getAppByName(client, accountDetails, name),
    rsconnect_app_not_found = function(err) NULL
  )
}

#' List the content of an account
#'
#' @param client A client object.
#' @param accountId The id of the account.
#' @param filters A named list of filters. Every client supports `name`, which
#'   must match exactly.
#'
#' @return A list of content items. Each item has an `id` and a `name`.
#' @noRd
listApplications <- function(client, accountId, filters = list()) {
  UseMethod("listApplications")
}

#' Get content by its id
#'
#' @param client A client object.
#' @param applicationId The content id from the deployment record.
#'
#' @return The content. If the content does not exist, the method signals an
#'   `rsconnect_http_404` error.
#' @noRd
getApplication <- function(client, applicationId) {
  UseMethod("getApplication")
}

#' Invite a user to an application
#'
#' @param client A client object.
#' @param applicationId The content id from `resolveContentTarget()`.
#' @param email The email address of the user.
#' @param sendEmail If `TRUE`, send an invitation email. `NULL` uses the server
#'   default.
#' @param emailMessage A message for the invitation email, or `NULL`.
#' @noRd
inviteApplicationUser <- function(
  client,
  applicationId,
  email,
  sendEmail = NULL,
  emailMessage = NULL
) {
  UseMethod("inviteApplicationUser")
}

#' Remove a user from an application
#'
#' @param client A client object.
#' @param applicationId The content id from `resolveContentTarget()`.
#' @param userId The id of the user from `listCollaborators()`.
#' @noRd
removeApplicationUser <- function(client, applicationId, userId) {
  UseMethod("removeApplicationUser")
}

#' Send an invitation again
#'
#' @param client A client object.
#' @param invitationId The id of the invitation from `listInvitations()`.
#' @param regenerate If `TRUE`, make a new invitation code.
#' @noRd
resendApplicationInvitation <- function(
  client,
  invitationId,
  regenerate = FALSE
) {
  UseMethod("resendApplicationInvitation")
}
