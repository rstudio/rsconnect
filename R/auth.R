cleanupPasswordFile <- function(appDir) {

  # normalize appDir path and ensure it exists
  appDir <- normalizePath(appDir, mustWork = FALSE)
  if (!file.exists(appDir) || !file.info(appDir)$isdir)
    stop(appDir, " is not a valid directory", call. = FALSE)
  
  # get data dir from appDir
  dataDir <- file.path(appDir, "shinyapps")

  # get password file
  passwordFile <- file.path(dataDir, paste("passwords", ".txt", sep=""))
  
  # check if password file exists
  if (file.exists(passwordFile)) {
    message("WARNING: Password file found! This application is configured to use scrypt ",
            "authentication, which is no longer supported.\nIf you choose to proceed, ",
            "all existing users of this application will be removed, ",
            "and will NOT be recoverable.\nFor for more information please visit: ",
            "http://shiny.rstudio.com/articles/migration.html")
    response <- readline("Do you want to proceed? [Y/n]: ")
    if (tolower(substring(response, 1, 1)) != "y") {
      stop("Cancelled", call. = FALSE)
    } else {
      # remove old password file
      file.remove(passwordFile)
    }
  }
  
  invisible(TRUE)
}

#' Add authorized user to application
#' 
#' @param email Email address of user to add.
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param appName Name of application.
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @seealso \code{\link{removeAuthorizedUser}} and \code{\link{showUsers}}
#' @export
addAuthorizedUser <- function(email, appDir=getwd(), appName=NULL, 
                              account = NULL, sendEmail=TRUE) {

  # resolve target account and application
  if (is.null(appName)) {
    appName = basename(appDir)
  }
  accountInfo <- accountInfo(resolveAccount(account))
  application <- resolveApplication(accountInfo, appName)

  # check for and remove password file
  cleanupPasswordFile(appDir)
  
  # fetch authoriztion list
  api <- lucidClient(accountInfo)
  api$inviteApplicationUser(application$id, validateEmail(email))

  message(paste("Added:", email, "to application", sep=" "))
  
  invisible(TRUE)
}

#' Remove authorized user from an application
#' 
#' @param user The user to remove. Can be id or email address.
#' @param appDir Directory containing application. Defaults to 
#' current working directory.
#' @param appName Name of application.
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @seealso \code{\link{addAuthorizedUser}} and \code{\link{showUsers}}
#' @export
removeAuthorizedUser <- function(user, appDir=getwd(), appName=NULL, 
                                 account = NULL) {
  
  # resolve target account and application
  if (is.null(appName)) {
    appName = basename(appDir)
  }
  accountInfo <- accountInfo(resolveAccount(account))
  application <- resolveApplication(accountInfo, appName)
  
  # check and remove password file
  cleanupPasswordFile(appDir)
  
  # get users
  users <- showUsers(appDir, appName, account)
  
  if (is.numeric(user)) {
    # lookup by id
    if (user %in% users$id) {
      user = users[users$id==user, ]
    } else {
      stop("User ", user, " not found", call. = FALSE)
    }
  } else {
    # lookup by email
    if (user %in% users$email) {
      user = users[users$email==user, ]
    } else {
      stop("User \"", user, "\" not found", call. = FALSE)
    }
  }
  
  # remove user
  api <- lucidClient(accountInfo)
  api$removeApplicationUser(application$id, user$id)
  
  message(paste("Removed:", user$email, "from application", sep=" "))
  
  invisible(TRUE)
}

#' List authorized users for an application
#' 
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param appName Name of application.
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @seealso \code{\link{addAuthorizedUser}} and \code{\link{showInvited}}
#' @export
showUsers <- function(appDir=getwd(), appName=NULL, account = NULL) {
  
  # resolve target account and application
  if (is.null(appName)) {
    appName = basename(appDir)
  }
  accountInfo <- accountInfo(resolveAccount(account))
  application <- resolveApplication(accountInfo, appName)

  # fetch authoriztion list
  api <- lucidClient(accountInfo)
  res <- api$listApplicationAuthoization(application$id)

  # get interesting fields
  users <- lapply(res, function(x) {
    a = list()
    a$id = x$user$id
    a$email = x$user$email
    if (!is.null(x$account)) {
      a$account <- x$account
    } else {
      a$account <- NA
    }
    return(a)
  })
  
  # convert to data frame
  users <- do.call(rbind, users)
  df <- as.data.frame(users, stringsAsFactors = FALSE)
  return(df)
}
  
#' List invited users for an application
#' 
#' @param appDir Directory containing application. Defaults to 
#'   current working directory.  
#' @param appName Name of application.
#' @param account Account name. If a single account is registered on the 
#'   system then this parameter can be omitted.
#' @seealso \code{\link{addAuthorizedUser}} and \code{\link{showUsers}}
#' @export
showInvited <- function(appDir=getwd(), appName=NULL, account = NULL) {

  # resolve target account and application
  if (is.null(appName)) {
    appName = basename(appDir)
  }
  accountInfo <- accountInfo(resolveAccount(account))
  application <- resolveApplication(accountInfo, appName)
  
  # fetch invitation list
  api <- lucidClient(accountInfo)
  res <- api$listApplicationInvitations(application$id)

  # get intersting fields 
  users <- lapply(res, function(x) {
    a = list()
    a$id = x$id
    a$email = x$email
    a$link = x$link
    return(a)
  })
  
  # convert to data frame
  users <- do.call(rbind, users)
  df <- as.data.frame(users, stringsAsFactors = FALSE)
  return(df)
}

#' (Deprecated) List authorized users for an application
#' 
#' @param appDir Directory containing application. Defaults to current working
#'  directory.
#' @export
authorizedUsers <- function(appDir = getwd()) {
  .Deprecated("showUsers")
  
  # read password file
  path <- getPasswordFile(appDir)
  if (file.exists(path)) {
    passwords <- readPasswordFile(path)
  } else {
    passwords <- NULL
  }
  
  return(passwords)
}

validateEmail <- function(email) {

  if (is.null(email) || !grepl(".+\\@.+\\..+", email)) {
    stop("Invalid email address.", call. = FALSE)
  }
  
  invisible(email)
}

validateUsername <- function(username) {
  
  # validate username length
  if (is.null(username) || nchar(username) < 1) {
    stop("Username must be at least 1 characters.", call. = FALSE)
  }
  
  # validate password has no invalid characeters 
  invalid <- c(":", "$", "\n", "\r")
  if (any(lapply(invalid, grepl, username, fixed = TRUE)==TRUE)) {
    stop("Username may not contain: $, :, \\n, or \\r", call. = FALSE)
  }
     
  invisible(TRUE)
}

validatePassword <- function(password) {
  
  min.length <- getOption('shinyapps.min.password.length', 4)
  
  # validate password length
  if (is.null(password) || nchar(password) < min.length) {
    stop("Password must be at least ", min.length, " characters.", call. = FALSE)
  }
  
  # validate password has no invalid characeters 
  invalid <- c(":", "$", "\n", "\r")
  if (any(lapply(invalid, grepl, password, fixed = TRUE)==TRUE)) {
    stop("Password may not contain: $, :, \\n, or \\r", call. = FALSE)
  }
  
  invisible(TRUE) 
}

promptPassword <- function() {
  prompt <- "Password: "
  password.one <- readPassword(prompt)
  prompt <- "Retype Password: "
  password.two <- readPassword(prompt)
  if (!identical(password.one, password.two)) {
    stop("Passwords do not match.", call. = FALSE)  
  }
  return(password.one)
}

getPasswordFile <- function(appDir) {
  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))
  
  # normalize appDir path and ensure it exists
  appDir <- normalizePath(appDir, mustWork = FALSE)
  if (!file.exists(appDir) || !file.info(appDir)$isdir)
    stop(appDir, " is not a valid directory", call. = FALSE)
  
  dataDir <- file.path(appDir, "shinyapps")
  if (!file.exists(dataDir))
    dir.create(dataDir, recursive=TRUE)
  
  passwordFile <- file.path(dataDir, paste("passwords", ".txt", sep=""))
  return(passwordFile)
}

readPasswordFile <- function(path) {
  # open and read file
  lines <- readLines(path)
  
  # extract fields
  fields <- do.call(rbind, strsplit(lines, ":"))
  users <- fields[,1]
  hashes <- fields[,2]
  
  # convert to data frame
  df <- data.frame(user=users, hash=hashes, stringsAsFactors=FALSE)

  # return data frame
  return(df)
}

writePasswordFile <- function(path, passwords) {
  
  # open and file
  f = file(path, open="w")
  on.exit(close(f), add = TRUE)
  
  # write passwords
  apply(passwords, 1, function(r) {
    l <- paste(r[1], ":", r[2], "\n", sep="")
    cat(l, file=f, sep="")
  })
  message("Password file updated. You must deploy your application for these changes to take effect.")
}
