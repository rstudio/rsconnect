
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
#' \code{status} \tab Current status of application. Valid values are 
#' \code{pending}, \code{deploying}, \code{running}, \code{terminating}, and
#' \code{terminated}.
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
  apps <- lucid$listApplications(accountInfo$accountId)
  
  # extract the subset of fields we're interested in 
  res <- lapply(apps, `[`, c('id', 'name', 'url', 'status', 'created_time', 
                             'updated_time', 'deployment'))
  
  # promote the size and instance data to first-level fields
  res <- lapply(res, function(x) {
    if (! is.null(x$deployment)) {
      x$size <- x$deployment$properties$application.instances.template
      x$instances <- x$deployment$properties$application.instances.count
    } else {
      x$size <- NA 
      x$instances <- NA 
    }
    x$deployment <- NULL
    return(x)
  })
  
  # convert to data frame
  res <- do.call(rbind, res)

  as.data.frame(res, stringsAsFactors = FALSE)
}

resolveApplication <- function(accountInfo, appName) {
  lucid <- lucidClient(accountInfo)
  apps <- lucid$listApplications(accountInfo$accountId)
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
  lucid$waitForTask(task$task_id, quiet)
  displayStatus(paste(taskDef$endStatus, "\n", sep = ""))
  
  invisible(NULL)
}

#' Show Application Logs
#' 
#' Show the logs of an application.
#' @export
showLogs <- function(appDir = getwd(), appName = NULL, account = NULL){
  
  # determine the log target and target account info
  target <- deploymentTarget(appDir, appName, account)
  accountInfo <- accountInfo(target$account)  
  lucid <- lucidClient(accountInfo)
  application <- getAppByName(lucid, accountInfo, target$appName)
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")
    
  # retreive logs
  logs <- lucid$getLogs(application$id)
  cat(logs)
}