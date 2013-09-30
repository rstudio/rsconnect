

#' Account Management Functions
#' 
#' Functions to add, enumerate, and remove ShinyApps accounts on the local
#' system.
#' @param name account name
#' @param key account key
#' @param secret account secret
#' @rdname accounts
#' @export
accounts <- function() {
  tools::file_path_sans_ext(list.files(accountsConfigDir(), 
                                       pattern=glob2rx("*.dcf")))
}


#' @rdname accounts
#' @export
setAccountInfo <- function(name, key, secret) {
  
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))
  
  if (!isStringParam(key))
    stop(stringParamErrorMessage("key"))
  
  if (!isStringParam(secret))
    stop(stringParamErrorMessage("secret"))
  
  write.dcf(list(name = name, key = key, secret = secret), 
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

