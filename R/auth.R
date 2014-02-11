#' Add authorized user for application
#' 
#' @param username 
#' @param password 
#' @param appDir Directory containing application. Defaults to current working directory.
#' @examples
#' \dontrun{
#' 
#' # add a user (prompts for password)
#' addAuthroizedUser("andy")
#' 
#' # add a user using supplied password
#' addAuthorizedUser("tareef", "MrShiny45")
#' 
#' }
#' @seealso \code{\link{removeAuthorizedUser}} and \code{\link{authorizedUsers}}
#' @export
addAuthorizedUser <- function(username, password = NULL, appDir = getwd()) {
  
  if (!require(scrypt)) {
    stop("scrypt package is not installed.")
  }
  
  if (missing(username) || !isStringParam(username))
    stop(stringParamErrorMessage("username"))
  
  # validate username
  validateUsername(username)
  
  # prompt for password if not given
  if (is.null(password)) {
    password <- promptPassword()
  }

  # validate password
  validatePassword(password)
  
  # hash password
  hash <- paste("{scrypt}", hashPassword(password), sep="")
  
  # read password file
  path <- getPasswordFile(appDir)
  if (file.exists(path)) {
    passwords <- readPasswordFile(path)
  } else {
    passwords <- NULL
  }
  
  # check if username is already in password list
  if (username %in% passwords$user) {
    # promp to reset password
    prompt <- paste("Reset password for user \"", username, "\"? [Y/n] ", sep="")
    input <- readline(prompt)
    if (nzchar(input) && !identical(tolower(input), "y")) {
      stop("Password not updated", call. = FALSE)
    } else {
      # update pasword
      passwords[passwords$user==username, "hash"] <- hash 
    }
  } else {
    # add row to data frame
    row <- data.frame(user=username, hash=hash, stringsAsFactors=FALSE)
    passwords <- rbind(passwords, row)
  }

  # write passwords
  invisible(writePasswordFile(path, passwords))
}

#' Remove authroized user from an application
#' 
#' @param username
#' @param appDir Directory containing application. Defaults to current working directory.
#' @examples
#' \dontrun{
#' 
#' # remove user 
#' removeAuthroizedUser("andy")
#' 
#' }
#' @seealso \code{\link{addAuthorizedUser}} and \code{\link{authorizedUsers}}
#' @export
removeAuthorizedUser <- function(username, appDir = getwd()) {
  
  # read password file
  path <- getPasswordFile(appDir)
  if (file.exists(path)) {
    passwords <- readPasswordFile(path)
  } else {
    passwords <- NULL
  }

  # check if username is already in password list
  if (!username %in% passwords$user) {
    stop("User \"", username, "\" not found", call. = FALSE)
  }

  # remove user 
  passwords <- passwords[passwords$user!=username, ]

  # write passwords
  invisible(writePasswordFile(path, passwords))
}

#' List authorized users for an application
#' 
#' @param appDir Directory containing application. Defaults to current working directory.
#' @export
authorizedUsers <- function(appDir = getwd()) {
  
  # read password file
  path <- getPasswordFile(appDir)
  if (file.exists(path)) {
    passwords <- readPasswordFile(path)
  } else {
    passwords <- NULL
  }
  
  return(passwords)
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