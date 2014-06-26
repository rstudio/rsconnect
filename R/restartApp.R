#' Restart an Application
#' 
#' Restart an application currently running on ShinyApps.
#' @param appName Name of application to restart
#' @param account Account name. If a single account is registered on the 
#' system then this parameter can be omitted.
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
restartApp <- function(appName, account = NULL, quiet = FALSE) {
  
  # define deploy task
  taskDef <- list()
  taskDef$beginStatus <- "Restarting application"
  taskDef$endStatus <- "Application successfully restarted"
  taskDef$action <- function(lucid, application) {
    lucid$deployApplication(application$id)
  }
  
  # perform it
  applicationTask(taskDef, appName, account, quiet)
}

