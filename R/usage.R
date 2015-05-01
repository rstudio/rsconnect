#' Show Application Usage
#' 
#' Show application usage of a currently deployed application 
#' @param appName Name of application 
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @param usageType
#' @param from
#' @param until
#' @param interval
#' @export
showUsage <- function(appDir=getwd(), appName=NULL, account = NULL, 
                      usageType="hours", from=NULL, until=NULL, interval=NULL) {
  
  # determine the target and target account info
  target <- deploymentTarget(appDir, appName, account)
  
  # get account
  accountInfo <- accountInfo(target$account)  
  
  # get application 
  lucid <- lucidClient(accountInfo)
  application <- getAppByName(lucid, accountInfo, appName) 
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")
  
  # get application usage
  data <- lucid$getAccountUsage(accountInfo$accountId, 
                                usageType,
                                application$id, 
                                from, 
                                until, 
                                interval)
  
  # get data points
  points <- data$points[[1]]
  points <- lapply(points, function(X) {
    X[[1]] <- X[[1]]/1000 # convert from milliseconds to seconds
    X
  })

  # convert to data frame
  df <- data.frame(matrix(unlist(points), nrow=length(points), byrow=T), stringsAsFactors=FALSE)
  colnames(df) <- c("timestamp", usageType)
  return(df)
}

#' Show Account Usage
#' 
#' Show account usage 
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @param usageType
#' @param from
#' @param until
#' @param interval
#' @export
accountUsage <- function(account=NULL, usageType="hours", 
                         from=NULL, until=NULL, interval=NULL) {
  
  # determine the target and target account info
  accountInfo <- accountInfo(account)  
  
  # get application 
  lucid <- lucidClient(accountInfo)
  
  # get application usage
  data <- lucid$getAccountUsage(accountInfo$accountId, 
                                usageType,
                                NULL,
                                from, 
                                until, 
                                interval)
}