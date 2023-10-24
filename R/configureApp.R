#' Configure an Application
#'
#' Configure an application running on a remote server.
#'
#' @inheritParams deployApp
#'
#' @param appName Name of application to configure
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @inheritParams deployApp
#' @param redeploy Re-deploy application after its been configured.
#' @param size Configure application instance size
#' @param instances Configure number of application instances
#' @examples
#' \dontrun{
#'
#' # set instance size for an application
#' configureApp("myapp", size="xlarge")
#' }
#' @seealso [applications()], [deployApp()]
#' @note This function works only for ShinyApps servers.
#' @export
configureApp <- function(appName, appDir = getwd(), account = NULL, server = NULL,
                         redeploy = TRUE, size = NULL,
                         instances = NULL, logLevel = c("normal", "quiet", "verbose")) {

  accountDetails <- accountInfo(account, server)
  checkShinyappsServer(accountDetails$server)

  if (is.null(appName))
    appName <- basename(appDir)
  application <- resolveApplication(accountDetails, appName)

  displayStatus <- displayStatus(identical(logLevel, "quiet"))

  # some properties may required a rebuild to take effect
  rebuildRequired <- FALSE

  # get a list of properties to set
  properties <- list()
  if (! is.null(size)) {
    properties[["application.instances.template"]] <- size
  }
  if (! is.null(instances)) {
    properties[["application.instances.count"]] <- instances
  }

  # set application properties
  client <- clientForAccount(accountDetails)
  for (i in names(properties)) {
    propertyName <- i
    propertyValue <- properties[[i]]

    # dispatch to the appropriate client implementation
    if (is.function(client$configureApplication))
      client$configureApplication(application$id, propertyName, propertyValue)
    else if (is.function(client$setApplicationProperty))
      client$setApplicationProperty(application$id, propertyName, propertyValue)
    else
      stop("Server ", accountDetails$server, " has no appropriate configuration method.")
  }

  # redeploy application if requested
  if (redeploy) {
    if (length(properties) > 0) {
      deployApp(appDir = appDir, appName = appName, account = account, logLevel = logLevel, upload = rebuildRequired)
    }
    else
    {
      displayStatus("No configuration changes to deploy")
    }
  }
}

#' Set Application property
#'
#' Set a property on currently deployed ShinyApps application.
#'
#' @param propertyName Name of property
#' @param propertyValue Property value
#' @param appName Name of application
#' @param appPath Directory or file that was deployed. Defaults to current
#'   working directory.
#' @inheritParams deployApp
#' @param force Forcibly set the property
#'
#' @note This function only works for ShinyApps servers.
#'
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
setProperty <- function(propertyName, propertyValue, appPath = getwd(),
                        appName = NULL, account = NULL, server = NULL, force = FALSE) {

  deployment <- findDeployment(
    appPath = appPath,
    appName = appName,
    server = server,
    account = account
  )
  accountDetails <- accountInfo(deployment$account, deployment$server)
  checkShinyappsServer(accountDetails$server)

  client <- clientForAccount(accountDetails)
  application <- getAppByName(client, accountDetails, deployment$name)

  invisible(client$setApplicationProperty(application$id,
                                         propertyName,
                                         propertyValue,
                                         force))
}

#' Unset Application property
#'
#' Unset a property on currently deployed ShinyApps application (restoring to
#' its default value)
#'
#' @inheritParams setProperty
#' @param force Forcibly unset the property
#'
#' @note This function only works for ShinyApps servers.
#'
#' @examples
#' \dontrun{
#'
#' # unset application package cache property to revert to default
#' unsetProperty("application.package.cache")
#'
#' }
#' @export
unsetProperty <- function(propertyName, appPath = getwd(), appName = NULL,
                          account = NULL, server = NULL, force = FALSE) {

  deployment <- findDeployment(
    appPath = appPath,
    appName = appName,
    server = server,
    account = account
  )
  accountDetails <- accountInfo(deployment$account, deployment$server)
  checkShinyappsServer(accountDetails$server)

  client <- clientForAccount(accountDetails)
  application <- getAppByName(client, accountInfo, deployment$name)

  invisible(client$unsetApplicationProperty(application$id,
                                           propertyName,
                                           force))
}


#' Show Application property
#'
#' Show properties of an application deployed to ShinyApps.
#'
#' @param appName Name of application
#' @param appPath Directory or file that was deployed. Defaults to current
#'   working directory.
#' @inheritParams deployApp
#'
#' @note This function works only for ShinyApps servers.
#'
#' @export
showProperties <- function(appPath = getwd(), appName = NULL, account = NULL, server = NULL) {

  deployment <- findDeployment(
    appPath = appPath,
    appName = appName,
    account = account,
    server = server
  )
  accountDetails <- accountInfo(deployment$account, deployment$server)
  checkShinyappsServer(accountDetails$server)

  client <- clientForAccount(accountDetails)
  application <- getAppByName(client, accountDetails, deployment$name)

  # convert to data frame
  res <- do.call(rbind, application$deployment$properties)
  df <- as.data.frame(res, stringsAsFactors = FALSE)
  names(df) <- c("value")
  return(df)
}
