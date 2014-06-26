#' Terminate an Application
#' 
#' Terminate an application currently running on ShinyApps.
#' @param appName Name of application to terminate
#' @param account Account name. If a single account is registered on the 
#' system then this parameter can be omitted.
#' @param quiet Request that no status information be printed to the console 
#'   during the termination.
#' @examples
#' \dontrun{
#' 
#' # terminate an application
#' terminateApp("myapp")
#' }
#' @seealso \code{\link{applications}}, \code{\link{deployApp}}, and
#'   \code{\link{restartApp}}
#' @export
terminateApp <- function(appName, account = NULL, quiet = FALSE) {
 
  # define terminate task
  taskDef <- list()
  taskDef$beginStatus <- "Terminating application"
  taskDef$endStatus <- "Application successfully terminated"
  taskDef$action <- function(lucid, application) {
    lucid$terminateApplication(application$id)
  }
  
  # perform it
  applicationTask(taskDef, appName, account, quiet)
}

