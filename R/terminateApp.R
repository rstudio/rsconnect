#' Terminate an Application
#'
#' Terminate and archive a currently deployed ShinyApps application.
#'
#' @param appName Name of application to terminate
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
#' # terminate an application
#' terminateApp("myapp")
#' }
#' @seealso [applications()], [deployApp()], and
#'   [restartApp()]
#' @export
terminateApp <- function(appName, account = NULL, server = NULL,
                         quiet = FALSE) {

  # define terminate task
  taskDef <- list()
  taskDef$beginStatus <- "Terminating application"
  taskDef$endStatus <- "Application successfully terminated"
  taskDef$action <- function(client, application) {
    client$terminateApplication(application$id)
  }

  # perform it
  applicationTask(taskDef, appName, account, server = server, quiet)
}
