

#' Account Management Functions
#' 
#' Functions to add, enumerate, and remove ShinyApps accounts on the local
#' system.
#' @rdname accounts
#' @export
accounts <- function() {
  tools::file_path_sans_ext(list.files(accountsConfigDir(), 
                                       pattern=glob2rx("*.dcf")))
}


#' @rdname accounts
#' @export
setAccountInfo <- function(name, key, secret) {
  
  # TODO: validate inputs
  
  write.dcf(list(name = name, key = key, secret = secret), 
            accountConfigFile(name))
}

#' @rdname accounts
#' @export
accountInfo <- function(name) {
  
  configFile <- accountConfigFile(name)
  if (!file.exists(configFile))
    stop("Account named '", name, "' does not exist")
  
  accountDcf <- read.dcf(accountConfigFile(name), all=TRUE)
  as.list(accountDcf)
}


#' @rdname accounts
#' @export
removeAccount <- function(name) {
  configFile <- accountConfigFile(name)
  if (!file.exists(configFile))
    stop("Account named '", name, "' does not exist")
  
  invisible(file.remove(configFile))
}


accountConfigFile <- function(name) {
  normalizePath(file.path(accountsConfigDir(), paste(name, ".dcf", sep="")),
                mustWork = FALSE)
}

accountsConfigDir <- function() {
  shinyappsConfigDir("accounts")
}
