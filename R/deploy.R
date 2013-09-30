
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
#' @export
deploy <- function(appDir = getwd(), appName = NULL, account = NULL) {
  
  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))
  
  if (!is.null(appName) && !isStringParam(appName))
    stop(stringParamErrorMessage("appName"))
  
  # read any existing deployments and see if there is a single deployment
  # that we can use for defaulting
  deployments <- readDeployments(appDir)
  defaultDeployment = ifelse(nrow(deployments) == 1, 
                             as.list(deployments[1,]), NULL)
  
  
  bundle <- bundleApp(appDir, appName)
  
  # now I attempt to create the application POST /applications/
  # this provides an application ID which I can /upload, /deploy, etc.
  
  
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
