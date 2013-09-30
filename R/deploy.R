
#' Deploy an application to ShinyApps
#' 
#' Deploy an application to ShinyApps
#' @param appDir Directory containing application source code (defaults to
#'   current working directory)
#' @param appName Name of application (names must be unique with ShinyApps 
#'   accounts) 
#' @param account ShinyApps account to deploy application to. This parameter is
#'   only required for the initial deployment of an application when there are
#'   multiple accounts configured on the system.
#' @param http Method to be used for uploading. \code{"rcurl"} uses the RCurl
#'   package to create a secure https connection; \code{"curl"} uses the curl
#'   system utility to create a secure https connection; \code{"insecure"} 
#'   creates an insecure http socket connection; The global default behavior can
#'   be configured by setting the \code{shinyapps.http} option (the default is 
#'   \code{"rcurl"}).
#' @export
deploy <- function(appDir = getwd(), 
                   appName = NULL,
                   account = NULL,
                   http = getOption("shinyapps.http", "rcurl")) {
  
  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))
  
  if (!is.null(appName) && !isStringParam(appName))
    stop(stringParamErrorMessage("appName"))
  
  # read any existing deployments and see if there is a single deployment
  # that we can use for defaulting
  deployments <- readDeployments(appDir)
  defaultDeployment = ifelse(nrow(deployments) == 1), 
                             as.list(deployments[1,]), NULL)
  
  
  bundle <- bundleApp(appDir, appName)


}



saveDeployment <- function(appDir, name, account, bundleId, url) {
  deployments <- rbind(readDeployments(appDir),
                       deploymentRecord(name, account, bundleId, url))
  write.dcf(deployments, deploymentsFile(appDir))
  invisible(NULL)
}

readDeployments <- function(appDir) {
  deployments <- deploymentsFile(appDir)
  if (file.exists(deployments))
    read.dcf(deployments)
  else
    deploymentRecord(name = character(),
                     account = character(),
                     bundleId = character(),
                     url = character())
}

deploymentsFile <- function(appDir) {
  shinyappsDir <- file.path(appDir, "shinyapps")
  if (!file.exists(shinyappsDir))
    dir.create(shinyappsDir)
  file.path("shinyapps", "DEPLOYMENTS")
}

deploymentRecord <- function(name, account, bundleId, url) {
  data.frame(name = name,
             account = account,
             bundleId = bundleId,
             url = url,
             stringsAsFactors = FALSE)
}
