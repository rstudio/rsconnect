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
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @param sendEmail Send an email letting the user know the application
#'   has been shared with them.
#' @seealso \code{\link{removeAuthorizedUser}} and \code{\link{showUsers}}
#' @note This function works only for ShinyApps servers.
#' @export
addAuthorizedUser <- function(email, appDir=getwd(), appName=NULL,
                              account = NULL, server=NULL, sendEmail=NULL) {

  # resolve account
  accountDetails <- accountInfo(resolveAccount(account, server), server)

  # resolve application
  if (is.null(appName))
    appName = basename(appDir)
  application <- resolveApplication(accountDetails, appName)

  # check for and remove password file
  cleanupPasswordFile(appDir)

  # fetch authoriztion list
  api <- clientForAccount(accountDetails)
  api$inviteApplicationUser(application$id, validateEmail(email), sendEmail)

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
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @seealso \code{\link{addAuthorizedUser}} and \code{\link{showUsers}}
#' @note This function works only for ShinyApps servers.
#' @export
removeAuthorizedUser <- function(user, appDir=getwd(), appName=NULL,
                                 account = NULL, server=NULL) {

  # resolve account
  accountDetails <- accountInfo(resolveAccount(account, server), server)

  # resolve application
  if (is.null(appName))
    appName = basename(appDir)
  application <- resolveApplication(accountDetails, appName)

  # check and remove password file
  cleanupPasswordFile(appDir)

  # get users
  users <- showUsers(appDir, appName, account, server)

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
  api <- clientForAccount(accountDetails)
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
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @seealso \code{\link{addAuthorizedUser}} and \code{\link{showInvited}}
#' @note This function works only for ShinyApps servers.
#' @export
showUsers <- function(appDir=getwd(), appName=NULL, account = NULL,
                      server=NULL) {

  # resolve account
  accountDetails <- accountInfo(resolveAccount(account, server), server)

  # resolve application
  if (is.null(appName))
    appName = basename(appDir)
  application <- resolveApplication(accountDetails, appName)

  # fetch authoriztion list
  api <- clientForAccount(accountDetails)
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
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @seealso \code{\link{addAuthorizedUser}} and \code{\link{showUsers}}
#' @note This function works only for ShinyApps servers.
#' @export
showInvited <- function(appDir=getwd(), appName=NULL, account = NULL,
                        server=NULL) {

  # resolve account
  accountDetails <- accountInfo(resolveAccount(account, server), server)

  # resolve application
  if (is.null(appName))
    appName = basename(appDir)
  application <- resolveApplication(accountDetails, appName)

  # fetch invitation list
  api <- clientForAccount(accountDetails)
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

getPasswordFile <- function(appDir) {
  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))

  # normalize appDir path and ensure it exists
  appDir <- normalizePath(appDir, mustWork = FALSE)
  if (!file.exists(appDir) || !file.info(appDir)$isdir)
    stop(appDir, " is not a valid directory", call. = FALSE)

  file.path(appDir, "shinyapps", "passwords.txt")
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
