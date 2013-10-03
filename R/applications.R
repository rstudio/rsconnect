
#' List Deployed Applications
#' 
#' List all applications currently deployed to ShinyApps for a given account.
#' @param account Account name. If a single account is registered on the 
#' system then this parameter can be omitted.
#' @return 
#' Returns a data frame with the following columns:
#' \tabular{ll}{
#' \code{name} \tab Name of application \cr 
#' \code{url} \tab URL where application can be accessed\cr
#' \code{status} \tab Current status of application. Valid values are \code{pending},
#' \code{deploying}, \code{running}, \code{terminating}, \code{terminated}.
#' }
#' @note To register an account you call the \link{setAccountInfo} function.
#' @examples
#' \dontrun{
#' 
#' # list all applications for the default account
#' applications()
#' 
#' # list all applications for a specific account
#' applications("myaccount")
#' 
#' # view the list of applications in the data viewer
#' View(applications())
#' }
#' @seealso \code{\link{deployApp}}, \code{\link{terminateApp}}, and
#'   \code{\link{scaleApp}}
#' @export
applications <- function(account = NULL) {
  
  # resolve account and create lucid client
  accountInfo <- accountInfo(resolveAccount(account))
  lucid <- lucidClient(accountInfo)
  
  # retreive applications
  apps <- lucid$applications(accountInfo$accountId)
  
  # convert the list into a data frame with a subset of fields
  name <- character()
  url <- character()
  status <- character()
  for (app in apps) {
    name <- append(name, app$name)
    url <- append(url, app$url)
    status <- append(status, app$status)
  }
  data.frame(name = I(name),
             url = I(url),
             status = status)
}

resolveApplication <- function(accountInfo, appName) {
  lucid <- lucidClient(accountInfo)
  apps <- lucid$applications(accountInfo$accountId)
  for (app in apps) {
    if (identical(app$name, appName))
      return (app)
  }
  
  stopWithApplicationNotFound(appName)
}

stopWithApplicationNotFound <- function(appName) {
  stop(paste("No application named '", appName, "' is currently deployed",
             sep=""), call. = FALSE)
}

applicationTask <- function(taskDef, appName, account, quiet) {
  
  # get status function and display initial status
  displayStatus <- displayStatus(quiet)
  displayStatus(paste(taskDef$beginStatus, "...\n", sep=""))
  
  # resolve target account and application
  accountInfo <- accountInfo(resolveAccount(account))
  application <- resolveApplication(accountInfo, appName)
  
  # perform the action
  lucid <- lucidClient(accountInfo)
  task <- taskDef$action(lucid, application)
  lucid$waitForTaskCompletion(task$task_id, quiet)
  displayStatus(paste(taskDef$endStatus, "\n", sep = ""))
  
  invisible(NULL)
}

