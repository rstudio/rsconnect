#' Configure an Application
#'
#' Configure an application currently running on RStudio Connect.
#' @param appName Name of application to configure
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param account Account name. If a single account is registered on the
#'   system then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers (see \code{\link{servers}})
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
configureApp <- function(appName, appDir=getwd(), account = NULL, server = NULL,
                         redeploy = TRUE, size = NULL, instances = NULL,
                         quiet = FALSE) {

  # resolve target account and application
  accountDetails <- accountInfo(resolveAccount(account, server), server)
  application <- resolveApplication(accountDetails, appName)

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
  client <- clientForAccount(accountDetails)
  for (i in names(properties)) {
    propertyName <- i
    propertyValue <- properties[[i]]
    client$configureApplication(application$id, propertyName, propertyValue)
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
