#' Scale Application Instances
#'
#' Scale the number of parallel instances used for a deployed application.
#'
#' @param appName Name of application
#' @param instances Number of parallel instances to be used for the application
#' @param account Account name. If a single account is registered on the system
#'   then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers (see \code{\link{servers}})
#' @param quiet Request that no status information be printed to the console
#' @examples
#' \dontrun{
#'
#' # specify that an application use 3 instances
#' scaleApp("myapp" instances = 3)
#' }
#' @seealso \code{\link{applications}}, \code{\link{deployApp}}, and
#'   \code{\link{terminateApp}}
#' @export
scaleApp <- function(appName, instances, account = NULL, server = NULL,
                     quiet = FALSE) {

  # define task
  taskDef <- list()
  taskDef$beginStatus <- "Scaling application instances"
  taskDef$endStatus <- "Application instances successfully scaled"
  taskDef$action <- function(client, application) {
    client$scaleApplication(application$id, instances)
  }

  # perform it
  applicationTask(taskDef, appName, account, server, quiet)
}
