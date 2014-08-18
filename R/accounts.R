#' Account Management Functions
#'
#' Functions to add, enumerate, and remove RStudio Connect accounts on the local
#' system. Prior to deploying applications you need to register
#' your RStudio Connect account on the local system.
#'
#' You register an account using the \code{setAccountInfo} function. You can
#' subsequently remove the account using the \code{removeAccount} function.
#'
#' The \code{accounts} and \code{accountInfo} functions are provided for viewing
#' previously registered accounts.
#' @param name Name of RStudio Connect account to save or remove
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

#' Connect User Account
#'
#' Connect an RStudio Connect user account to the package, so that it can be
#' used to deploy and manage applications on behalf of the account.
#'
#' @param username An optional nickname for the account; applied only if the
#'   account doesn't have a nickname already set on RStudio Connect.
#' @param quiet Whether or not to show messages and prompts while connecting
#'   the account.
#'
#' @details When this function is invoked, a web browser will be launched on
#'   RStudio Connect, where you will be prompted to enter your credentials. Upon
#'   successful authentication, your local installation of \pkg{rsconnect} and
#'   your RStudio Connect account will be paired, and you'll be able to deploy
#'   and manage applications using the package without further prompts for
#'   credentials.
#'
#' @export
connectUser <- function(username = "", quiet = FALSE) {
  # generate a token and send it to the server
  token <- generateToken()
  connect <- connectClient(list())
  response <- connect$addToken(list(token = token$token,
                                    public_key = token$public_key))
  if (!quiet) {
    message("A browser window should open; if it doesn't, you may authenticate ",
            "manually by visiting ", response$token_claim_url, ".")
    message("Waiting for authentication...")
  }
  utils::browseURL(response$token_claim_url)

  # keep trying to authenticate until we're successful
  connect <- connectClient(list(token = token$token,
                                private_key = token$private_key))
  interrupted <- FALSE
  repeat {
    tryCatch({
      Sys.sleep(1)
      user <- connect$currentUser()
      break
    },
    error = function(e, ...) {
      # we expect this to return unauthorized until the token becomes active,
      # but bubble other errors
      if (length(grep("401 - Unauthorized", e$message)) == 0) {
        stop(e)
      }
    })
  }

  # populate the username if there wasn't one set on the server
  if (nchar(user$username) == 0) {
    if (nchar(username) > 0)
      user$username <- username
    else
      user$username <- tolower(paste0(substr(user$first_name, 1, 1),
                                      user$last_name))

    # in interactive mode, prompt for a username before accepting defaults
    if (!quiet && interactive() && nchar(username) == 0) {
      input <- readline(paste0("Choose a nickname for this account (default '",
                               user$username, "'): "))
      if (nchar(input) > 0)
        user$username <- input
    }
  }

  # write the user info
  configFile <- accountConfigFile(user$username)
  write.dcf(list(username = user$username,
                 accountId = user$id,
                 token = token$token,
                 private_key = as.character(token$private_key)),
            configFile)

  # set restrictive permissions on it if possible
  if (identical(.Platform$OS.type, "unix"))
    Sys.chmod(configFile, mode="0600")

  if (!quiet) {
    message("Account registered successfully: ", user$first_name, " ",
            user$last_name, " (", user$username, ")")
  }
}

#' @rdname accounts
#' @export
setAccountInfo <- function(name, token, private_key) {

  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))

  if (!isStringParam(token))
    stop(stringParamErrorMessage("token"))

  if (!isStringParam(private_key))
    stop(stringParamErrorMessage("private_key"))

  # create connect client
  authInfo <- list(token = token, private_key = private_key)
  connect <- connectClient(authInfo)

  # get user Id
  userId <- connect$currentUser()$id

  # get account id
  accountId <- NULL
  accounts <- connect$accountsForUser(userId)
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
                 private_key = private_key),
            configFile)

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
  info <- as.list(accountDcf)
  # remove all whitespace from private key
  if (!is.null(info$private_key)) {
    info$private_key <- gsub("[[:space:]]","",info$private_key)
  }
  info
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
  rsconnectConfigDir("accounts")
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
             "accounts registered on this system)", call. = FALSE))
}

stopWithMissingAccount <- function(account) {
  stop(missingAccountErrorMessage(account), call. = FALSE)
}

