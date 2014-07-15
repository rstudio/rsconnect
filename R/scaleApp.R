#' Scale Application Instances
#' 
#' Scale the number of parallel instances used for an application deployed to
#' ShinyApps.
#' @param appName Name of application
#' @param instances Number of parallel instances to be used for the application
#' @param account Account name. If a single account is registered on the system
#'   then this parameter can be omitted.
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
scaleApp <- function(appName, instances, account = NULL, quiet = FALSE) {
  
  # define task
  taskDef <- list()
  taskDef$beginStatus <- "Scaling application instances"
  taskDef$endStatus <- "Application instances successfully scaled"
  taskDef$action <- function(lucid, application) {
    lucid$scaleApplication(application$id, instances)
  }
  
  # perform it
  applicationTask(taskDef, appName, account, quiet)
}
