#' Restart an Application
#'
#' Restart an application currently running on a remote server.
#'
#' @param appName Name of application to restart
#' @param account Account name. If a single account is registered on the system
#'   then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers (see \code{\link{servers}})
#' @param quiet Request that no status information be printed to the console
#'   during the operation.
#' @examples
#' \dontrun{
#'
#' # restart an application
#' restartApp("myapp")
#' }
#' @seealso \code{\link{applications}}, \code{\link{deployApp}}, and
#'   \code{\link{terminateApp}}
#' @export
restartApp <- function(appName, account = NULL, server = NULL, quiet = FALSE) {

  # define deploy task
  taskDef <- list()
  taskDef$beginStatus <- "Restarting application"
  taskDef$endStatus <- "Application successfully restarted"
  taskDef$action <- function(client, application) {
    client$deployApplication(application$id)
  }

  # perform it
  applicationTask(taskDef, appName, account, server, quiet)
}

