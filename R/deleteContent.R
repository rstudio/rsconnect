#' Delete content
#'
#' @description
#' Permanently delete content from Posit Connect Cloud. The deletion cannot be
#' undone.
#'
#' Supported servers: Posit Connect Cloud
#'
#' @param appDir Directory containing the content's deployment record. Defaults
#'   to the current working directory.
#' @param appName Name of the deployment record to use when `appDir` has more
#'   than one.
#' @param contentId The content ID to delete, taken from the content URL
#'   (\code{https://connect.posit.cloud/{account}/content/{contentId}}). When
#'   supplied, `appDir` and `appName` are ignored and no local deployment
#'   record is required.
#' @param account,server Uniquely identify a remote server with either your
#'   user `account`, the `server` name, or both. If neither are supplied, and
#'   there are multiple options, you'll be prompted to pick one.
#' @param force If `FALSE` (the default), ask for confirmation before deleting.
#'   Set to `TRUE` to delete without asking, which is required in
#'   non-interactive sessions.
#' @note This function only works for Posit Connect Cloud. When the content is
#'   found through a local deployment record, that record is also removed.
#' @seealso [deployApp()], [applications()], and [forgetDeployment()]
#' @examples
#' \dontrun{
#'
#' # delete the content deployed from the current directory
#' deleteContent()
#'
#' # delete content by id, without asking for confirmation
#' deleteContent(contentId = "0192f1a2-...", force = TRUE)
#' }
#' @export
deleteContent <- function(
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
  account = NULL,
  server = NULL,
  force = FALSE
) {
  check_bool(force)
  accountDetails <- accountInfo(account, server)
  checkPositConnectCloudServer(accountDetails$server)

  target <- resolveContentTarget(accountDetails, appDir, appName, contentId)
  client <- clientForAccount(accountDetails)

  content <- withCallingHandlers(
    client$getContent(target$id),
    rsconnect_http_404 = function(err) {
      cli::cli_abort(
        "Can't find content with id {.str {target$id}}; it may already be deleted.",
        parent = err
      )
    }
  )

  if (!force) {
    cli_menu(
      "Content {.val {content$title}} ({target$id}) will be permanently deleted.",
      "Do you want to continue?",
      choices = c("Cancel", "Delete the content"),
      not_interactive = c(
        i = "Use {.code force = TRUE} to delete without confirmation."
      ),
      quit = 1
    )
  }

  client$deleteContent(target$id)
  if (!is.null(target$deploymentFile)) {
    unlink(target$deploymentFile)
  }
  cli::cli_inform(c(v = "Deleted content {.val {content$title}}."))

  invisible(TRUE)
}
