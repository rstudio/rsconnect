#' Configure an Application
#' 
#' Configure an application currently running on ShinyApps.
#' @param appName Name of application to configure
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @param redeploy Re-deploy application after its been configured.
#' @param size Configure application instance size
#' @param instances Configure number of application instances
#' @param quiet Request that no status information be printed to the console 
#'   during the deployment. 
#' @examples
#' \dontrun{
#' 
#' # set instance size for an application
#' configureApp("myapp", size="xlarge")
#' }
#' @seealso \code{\link{applications}}, \code{\link{deployApp}}
#' @export
configureApp <- function(appName, appDir=getwd(), account = NULL, 
                         redeploy = TRUE, size = NULL, 
                         instances = NULL, quiet = FALSE) {
  
  # resolve target account and application
  accountInfo <- accountInfo(resolveAccount(account))
  application <- resolveApplication(accountInfo, appName)

  displayStatus <- displayStatus(quiet) 
  
  # some properties may required a rebuild to take effect
  rebuildRequired = FALSE
  
  # get a list of properties to set
  properties <- list()
  if (! is.null(size) ) {
    properties[[ "application.instances.template" ]] = size
  }
  if (! is.null(instances) ) {
    properties[[ "application.instances.count" ]] = instances
  }

  # set application properties
  lucid <- lucidClient(accountInfo)
  for (i in names(properties)) {
    propertyName <- i
    propertyValue <- properties[[i]]
    lucid$setApplicationProperty(application$id, propertyName, propertyValue)
  }

  # redeploy application if requested
  if (redeploy) {
    if (length(properties) > 0) {
      deployApp(appDir=appDir, appName=appName, account=account, quiet=quiet, upload=rebuildRequired) 
    }
    else 
    {
      displayStatus("No configuration changes to deploy")
    }
  }
}

#' Set Application property 
#' 
#' Set a property on currently deployed application
#' @param propertyName Name of property to set
#' @param propertyValue Nalue to set property to
#' @param appName Name of application 
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @param force Forcibly set the property 
#' @examples
#' \dontrun{
#' 
#' # set instance size for an application
#' setProperty("application.instances.count", 1)
#' 
#' # disable application package cache
#' setProperty("application.package.cache", FALSE)
#' 
#' }
#' @export
setProperty <- function(propertyName, propertyValue, appDir=getwd(), 
                        appName=NULL, account = NULL, force=FALSE) {
  
  # resolve the application target and target account info
  target <- deploymentTarget(appDir, appName, account)
  accountInfo <- accountInfo(target$account)  
  lucid <- lucidClient(accountInfo)
  application <- getAppByName(lucid, accountInfo, target$appName)
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")
  
  invisible(lucid$setApplicationProperty(application$id, 
                                         propertyName, 
                                         propertyValue,
                                         force))
}

#' Unset Application property 
#' 
#' Unset a property on currently deployed application to restoring its default 
#' @param propertyName Name of property to unset
#' @param appName Name of application 
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @param force Forcibly unset the property 
#' @examples
#' \dontrun{
#' 
#' # unset application package cache property to revert to default
#' unsetProperty("application.package.cache")
#' 
#' }
#' @export
unsetProperty <- function(propertyName, appDir=getwd(), appName=NULL, 
                          account = NULL, force=FALSE) {
  
  # resolve the application target and target account info
  target <- deploymentTarget(appDir, appName, account)
  accountInfo <- accountInfo(target$account)  
  lucid <- lucidClient(accountInfo)
  application <- getAppByName(lucid, accountInfo, target$appName)
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")
  
  invisible(lucid$unsetApplicationProperty(application$id, 
                                           propertyName,
                                           force))
}


#' Show Application property 
#' 
#' Show application propreties of a currently deployed application 
#' @param appName Name of application 
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @export
showProperties <- function(appDir=getwd(), appName=NULL, account = NULL) {
  
  # determine the log target and target account info
  target <- deploymentTarget(appDir, appName, account)
  accountInfo <- accountInfo(target$account)  
  lucid <- lucidClient(accountInfo)
  application <- getAppByName(lucid, accountInfo, target$appName)
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")
  
  # convert to data frame
  res <- do.call(rbind, application$deployment$properties)
  df <- as.data.frame(res, stringsAsFactors = FALSE)
  names(df) <- c("value")
  return(df)
}
