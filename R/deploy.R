
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
  
  if (is.null(appName)) {
    appName <- basename(appDir)
  }
  
  bundle <- bundleApp(appDir, appName)

  system(paste("open", bundle))
}



