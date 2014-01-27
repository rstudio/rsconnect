
#' Add User
#' @export
addAuthorizedUser <- function(username, password = NULL, appDir = getwd(), appName = NULL, account = NULL, quiet = FALSE) {
  
  if (!require(scrypt)) {
    stop("scrypt package is not installed.")
  }
  
  if (missing(username) || !isStringParam(username))
    stop(stringParamErrorMessage("username"))
  
  # prompt for password if not given
  if (is.null(password)) {
    password <- promptPassword()
  }

  if (is.null(password) || !nzchar(password) || nchar(password) < getOption('shinyapps.min.password.length', 4)) {
    stop("Password must be at least ", getOption('shinyapps.min.password.length', 4), " characters.", call. = FALSE)
  }
  
  # hash password
  hash <- paste("{scrypt}", hashPassword(password), sep="")
  
  # read password file
  path <- passwordFilePath(appDir)
  if (file.exists(path)) {
    passwords <- readPasswordFile(path)
  } else {
    passwords <- NULL
  }
  
  # check if username is already in password list
  if (username %in% passwords$user) {
    prompt <- paste("Reset password for user \"", username, "\"? [Y/n] ", sep="")
    input <- readline(prompt)
    if (nzchar(input) && !identical(input, "y") && !identical(input, "Y")) {
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
  writePasswordFile(path, passwords)
  
}

#' Remove User
#' @export
removeAuthorizedUser <- function(username, appDir = getwd(), appName = NULL, account = NULL, quiet = FALSE ) {
  
  # read password file
  path <- passwordFilePath(appDir)
  passwords <- readPasswordFile(path)

  # check if username is already in password list
  if (!username %in% passwords$user) {
    stop("User \"", username, "\" not found", call. = FALSE)
  }

  # remove user 
  passwords <- passwords[passwords$user!=username, ]

  # write passwords
  writePasswordFile(path, passwords)
}

#' List Users
#' @export
authorizedUsers <- function(appDir = getwd()) {
  
  path <- passwordFilePath(appDir)
  passwords <- readPasswordFile(path)
  return (passwords)
}

promptPassword <- function() {
  prompt <- paste("Password: ")
  password.one <- readPassword(prompt)
  prompt <- paste("Retype Password: ")
  password.two <- readPassword(prompt)
  if (!identical(password.one, password.two)) {
    stop("Passwords do not match.", call. = FALSE)  
  }
  return(password.one)
}

passwordFilePath <- function(appDir) {
  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))
  
  # normalize appDir path and ensure it exists
  appDir <- normalizePath(appDir, mustWork = FALSE)
  if (!file.exists(appDir) || !file.info(appDir)$isdir)
    stop(appDir, " is not a valid directory")
  
  p <- paste(appDir, ".passwords.txt", sep="/")
  return(p)
}

readPasswordFile <- function(path) {
  # open and read file
  f <- file(path, open = "r")
  lines <- readLines(f)
  
  # extract fields
  fields <- do.call(rbind, strsplit(lines, ":"))
  users <- fields[,1]
  hashes <- fields[,2]
  
  # convert to data frame
  df <- data.frame(user=users, hash=hashes, stringsAsFactors=FALSE)

  close(f)

  # return data frame
  return(df)
}

writePasswordFile <- function(path, passwords) {
  # open and file
  f = file(path, open="w")
  
  # write passwords
  apply(passwords, 1, function(r) {
    l <- paste(r[1], ":", r[2], "\n", sep="")
    cat(l, file=f, sep="")
  })
  close(f)
  cat("Password file updated.")
}