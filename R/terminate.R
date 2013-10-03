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
#' terminate("myapp")
#' }
#' @export
terminate <- function(appName, account = NULL, quiet = FALSE) {
 
  # get status function and display initial status
  displayStatus <- displayStatus(quiet)
  displayStatus("Terminating application...\n")
  
  # resolve target account and application
  accountInfo <- accountInfo(resolveAccount(account))
  application <- resolveApplication(accountInfo, appName)
  
  # terminate the application
  lucid <- lucidClient(accountInfo)
  task <- lucid$terminateApplication(application$id)
  lucid$waitForTaskCompletion(task$task_id, quiet)
  displayStatus("Application successfully terminated.\n")
  
  invisible(NULL)
}