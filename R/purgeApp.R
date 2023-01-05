#' Purge an Application
#'
#' Purge a currently archived ShinyApps application.
#'
#' @param appName Name of application to purge
#' @param account Account name. If a single account is registered on the system
#'   then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers (see [servers()])
#' @param quiet Request that no status information be printed to the console
#'   during the termination.
#'
#' @note This function only works for ShinyApps servers.
#'
#' @examples
#' \dontrun{
#'
#' # purge an application
#' purgeApp("myapp")
#' }
#' @seealso [applications()], [deployApp()], and
#'   [restartApp()]
#' @export
purgeApp <- function(appName, account = NULL, server = NULL,
                     quiet = FALSE) {

  # define purge task
  taskDef <- list()
  taskDef$beginStatus <- "Purging application"
  taskDef$endStatus <- "Application successfully purged"
  taskDef$action <- function(client, application) {
    client$purgeApplication(application$id)
  }

  # perform it
  applicationTask(taskDef, appName, account, server = server, quiet)
}
