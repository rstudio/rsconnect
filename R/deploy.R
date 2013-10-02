
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
   
  # initialize lucid client
  lucid <- lucidClient(accountInfo)
  
  # determine the deployment target and implied account info
  target <- deploymentTarget(appDir, appName, account)
  accountInfo <- accountInfo(target$account)
    
  # ensure that the application exists
  app <- lucid$createApplication(target$appName, accountInfo$accountId, TRUE)
  
  # create the bundle and upload it 
  bundle <- bundleApp(appDir, target$appName)
  
 
}

# calculate the deployment target based on the passed parameters and 
# any saved deployments that we have
deploymentTarget <- function(appDir, appName, account) {
  
  # expand path of appDir
  appDir <- normalizePath(appDir)
  
  # read existing accounts 
  accounts <- accounts()
  if (length(accounts) == 0) {
    stop(paste("You must register an account using setAccountInfo prior to",
               "deploying an application."), call. = FALSE)
  }
  
  # validate account if provided
  if (!is.null(account)) {
    if (!account %in% accounts)
      stop(paste("Unknown account name '", account, "' (you can use the ",
                 "setAccountInfo function to add a new account)", sep = ""),
           call. = FALSE)
  }
  
  # read existing deployments 
  deployments <- readDeployments(appDir)
  
  # both appName and account explicitly specified
  if (!is.null(appName) && !is.null(account)) {
    
    list(appName = appName, account = account)
  
  }
  
  # just appName specified
  else if (!is.null(appName)) {
    
    # find any existing deployments of this application
    deployments <- readDeployments(appDir, nameFilter = appName)
    
    # if there are none then we can create it if there is a single account
    # registered that we can default to
    if (nrow(deployments) == 0) {
      if (length(accounts) == 1)
        list(appName = appName, account = accounts[[1]])
      else
        stop(paste("Please specify the account which you want to deploy the",
                   "application to."), call. = FALSE)
    }
    
    # single existing deployment
    else if (nrow(deployments) == 1) {
      list(appName = appName, account = deployments$account)
    }
    
    # multiple existing deployments
    else if (nrow(deployments) > 1) {
      stop(paste("Please specify the account you want to deploy '", appName,
                 "' to (you have previously deployed this application to ", 
                 "more than one account).",sep = ""), call. = FALSE)
    }
    
  }
           
  # just account specified, that's fine we just default the app name
  # based on the basename of the applicaton directory
  else if (!is.null(account)) {
        
    list(appName = basename(appDir), account = account)
  
  }

  # neither specified but a single existing deployment
  else if (nrow(deployments) == 1) {
     
    list(appName = deployments$name, account = deployments$account)
  
  }
  
  # neither specified and no existing deployments
  else if (nrow(deployments) == 0) {
    
    # single account we can default to
    if (length(accounts) == 1)
      list(appName = basename(appDir), account = accounts[[1]])
    else
      stop(paste("Please specify the account which you want to deploy the",
                 "application to (there is more than one account registered",
                 "on this system)."), call. = FALSE)
    
  }
  
  # neither specified and multiple existing deployments
  else {

    stop("Unable to deploy using default arguments (multiple existing ",
         "deployments from this application directory already exist). ",
         "Please specify appName and/or account name explicitly.", 
         call. = FALSE)
  
  }
}

saveDeployment <- function(appDir, name, account, bundleId, url) {
  
  deployment <- deploymentRecord(name, account, bundleId, url)
  write.dcf(deployment, deploymentFile(appDir, name, account))
  invisible(NULL)
}

readDeployments <- function(appDir, nameFilter = NULL, accountFilter = NULL) {
  
  deployments <- deploymentRecord(name = character(),
                                  account = character(),
                                  bundleId = character(),
                                  url = character())
  
  shinyappsDir <- file.path(appDir, "shinyapps")
  for (accountDir in file.path(shinyappsDir, list.files(shinyappsDir))) {
    
    # ignore regular files
    if (!file.info(accountDir)$isdir)
      next
    
    # apply optional account filter
    if (!is.null(accountFilter) && !identical(accountFilter, 
                                              basename(accountDir)))
      next
    
    deploymentFiles <- list.files(accountDir, glob2rx("*.dcf"))
    for (deploymentFile in file.path(accountDir, deploymentFiles)) {
      
      deployment <- as.data.frame(read.dcf(deploymentFile), 
                                  stringsAsFactors = FALSE)
      
      # apply optional name filter
      if (!is.null(nameFilter) && !identical(nameFilter, 
                                             basename(deploymentFile)))
        next
      
      deployments <- rbind(deployments, deployment)
    }
  }
  
  deployments
}

deploymentFile <- function(appDir, name, account) {
  accountDir <- file.path(appDir, "shinyapps", account)
  if (!file.exists(accountDir))
    dir.create(accountDir)
  file.path(accountDir, paste(name, ".dcf", sep=""))
}

deploymentRecord <- function(name, account, bundleId, url) {
  data.frame(name = name,
             account = account,
             bundleId = bundleId,
             url = url,
             stringsAsFactors = FALSE)
}
