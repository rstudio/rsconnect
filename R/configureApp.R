#' Configure an Application
#' 
#' Configure an application currently running on ShinyApps. Note, applications
#' should be re-deployed after being reconfigured. You can bypass application
#' upload by using the upload=FALSE parameter to deployApp.
#' @param appName Name of application to configure 
#' @param account Account name. If a single account is registered on the 
#' system then this parameter can be omitted.
#' @param size Configure application instance size
#' @param instances Configure number of application instances
#' @examples
#' \dontrun{
#' 
#' # set instance size for an application
#' configureApp("myapp", size="xlarge")
#' }
#' @seealso \code{\link{applications}}, \code{\link{deployApp}}
#' @export
configureApp <- function(appName, account = NULL, quiet = FALSE, size = NULL, instances = NULL) {
  
  # resolve target account and application
  accountInfo <- accountInfo(resolveAccount(account))
  application <- resolveApplication(accountInfo, appName)

  # get a list of properties to set
  properties <- list()
  if (! is.null(size) ) {
    properties[[ "containers.template" ]] = size
  }
  if (! is.null(instances) ) {
    properties[[ "containers.count" ]] = instances
  }

  # set application properties
  lucid <- lucidClient(accountInfo)
  for (i in names(properties)) {
    propertyName <- i
    propertyValue <- properties[[i]]
    lucid$configureApplication(application$id, propertyName, propertyValue)
  }
}
