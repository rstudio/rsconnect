#' Account Management Functions
#' 
#' Functions to add, enumerate, and remove ShinyApps accounts on the local 
#' system. Prior to deploying applications you need to register
#' your ShinyApps account on the local system.
#' 
#' You register an account using the \code{setAccountInfo} function. You can
#' subsequently remove the account using the \code{removeAccount} function.
#' 
#' The \code{accounts} and \code{accountInfo} functions are provided for viewing
#' previously registered accounts.
#' @param name Name of ShinyApps account to save or remove
#' @param token User token for the account
#' @param secret User secret for the account
#' @return
#' \code{accounts} returns a character vector with the names of all accounts
#' registered on the system. \code{accountInfo} returns a list with account
#' details.
#' @rdname accounts
#' @examples
#' \dontrun{
#' 
#' # register an account
#' setAccountInfo("user", "token", "secret")
#' 
#' # remove the same account
#' removeAccount("user")
#' }
#' @export
accounts <- function() {
  tools::file_path_sans_ext(list.files(accountsConfigDir(), 
                                       pattern=glob2rx("*.dcf")))
}


#' @rdname accounts
#' @export
setAccountInfo <- function(name, token, secret) {
    
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))
  
  if (!isStringParam(token))
    stop(stringParamErrorMessage("token"))
  
  if (!isStringParam(secret))
    stop(stringParamErrorMessage("secret"))

  # create lucid client
  authInfo <- list(token = token, secret = secret)
  lucid <- lucidClient(authInfo)
  
  # get user Id
  userId <- lucid$currentUser()$id
  
  # get account id
  accountId <- NULL
  accounts <- lucid$accountsForUser(userId)
  for (account in accounts) {
    if (identical(account$name, name)) {
      accountId <- account$id
      break
    }
  } 
  if (is.null(accountId))
    stop("Unable to determine account id for account named '", name, "'")
 
  # get the path to the config file
  configFile <- accountConfigFile(name)
  
  # write the user info
  write.dcf(list(name = name,
                 userId = userId,
                 accountId = accountId,
                 token = token, 
                 secret = secret), 
            configFile,
            width = 100)
  
  # set restrictive permissions on it if possible
  if (identical(.Platform$OS.type, "unix"))
    Sys.chmod(configFile, mode="0600")
}

#' @rdname accounts
#' @export
accountInfo <- function(name) {
  
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))
  
  configFile <- accountConfigFile(name)
  if (!file.exists(configFile))
    stop(missingAccountErrorMessage(name))
  
  accountDcf <- readDcf(accountConfigFile(name), all=TRUE)
  as.list(accountDcf)
}


#' @rdname accounts
#' @export
removeAccount <- function(name) {
  
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))
  
  configFile <- accountConfigFile(name)
  if (!file.exists(configFile))
    stop(missingAccountErrorMessage(name))
  
  file.remove(configFile)
  
  invisible(NULL)
}


accountConfigFile <- function(name) {
  normalizePath(file.path(accountsConfigDir(), paste(name, ".dcf", sep="")),
                mustWork = FALSE)
}

accountsConfigDir <- function() {
  shinyappsConfigDir("accounts")
}

missingAccountErrorMessage <- function(name) {
  paste("account named '", name, "' does not exist", sep="")
}

resolveAccount <- function(account) {
  
  # get existing accounts
  accounts <- accounts()
  if (length(accounts) == 0)
    stopWithNoAccount()
  
  # if no account was specified see if we can resolve the account to a default
  if (is.null(account)) {
    if (length(accounts) == 1) 
      accounts[[1]]
    else
      stopWithSpecifyAccount()
  }
  # account explicitly specified, confirm it exists
  else {
      if (account %in% accounts)
        account
      else
        stopWithMissingAccount(account)
  }
}

stopWithNoAccount <- function() {
  stop(paste("You must register an account using setAccountInfo prior to",
             "proceeding."), call. = FALSE)
}

stopWithSpecifyAccount <- function() {
  stop(paste("Please specify the account name (there are more than one",
             "accounts registered on this system)"), call. = FALSE)
}

stopWithMissingAccount <- function(account) {
  stop(missingAccountErrorMessage(account), call. = FALSE)
}

