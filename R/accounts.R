

#' Account Management Functions
#' 
#' Functions to add, enumerate, and remove ShinyApps accounts on the local
#' system.
#' @param name Account name
#' @param token User token
#' @param secret User token secret
#' @rdname accounts
#' @export
accounts <- function() {
  tools::file_path_sans_ext(list.files(accountsConfigDir(), 
                                       pattern=glob2rx("*.dcf")))
}


#' @rdname accounts
#' @export
setAccountInfo <- function(name, token, secret) {
  
  # TODO: token looks up a single user (we store the userId) 
  # Andy to add API for this
  # (hardcode to 4 if necessary)
  
  # TODO: name used to lookup accountId (we store this)
  #  GET /users/<id>/accounts/ with a filter
  #  https://api.shinyapps.io/v1/users/4/accounts?filter=name:eq:jjallaire
  # -- should be one that matches, error if not
  
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))
  
  if (!isStringParam(token))
    stop(stringParamErrorMessage("token"))
  
  if (!isStringParam(secret))
    stop(stringParamErrorMessage("secret"))
  
  write.dcf(list(name = name, token = token, secret = secret), 
            accountConfigFile(name))
}

#' @rdname accounts
#' @export
accountInfo <- function(name) {
  
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))
  
  configFile <- accountConfigFile(name)
  if (!file.exists(configFile))
    stop(missingAccountErrorMessage(name))
  
  accountDcf <- read.dcf(accountConfigFile(name), all=TRUE)
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
  
  invisible(file.remove(configFile))
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

