#' Account Management Functions
#'
#' Functions to enumerate and remove accounts on the local system. Prior to
#' deploying applications you need to register your account on the local system.
#'
#' You register an account using the \code{\link{setAccountInfo}} function (for
#' ShinyApps) or \code{\link{connectUser}} function (for other servers). You can
#' subsequently remove the account using the \code{removeAccount} function.
#'
#' The \code{accounts} and \code{accountInfo} functions are provided for viewing
#' previously registered accounts.
#'
#' @param name Name of account
#' @param server Name of the server on which the account is registered
#'   (optional; see \code{\link{servers}})
#'
#' @return \code{accounts} returns a data frame with the names of all accounts
#' registered on the system and the servers on which they reside.
#' \code{accountInfo} returns a list with account details.
#'
#' @rdname accounts
#' @export
accounts <- function(server = NULL) {
  path <- accountsConfigDir()
  if (!is.null(server))
    path <- file.path(path, server)

  # get a raw list of accounts
  accountnames <- tools::file_path_sans_ext(list.files(path,
    pattern=glob2rx("*.dcf"), recursive = TRUE))

  if (length(accountnames) == 0) {
    return(NULL)
  }

  # convert to a data frame
  servers <- dirname(accountnames)
  servers[servers == "."] <- "shinyapps.io"
  names <- fileLeaf(accountnames)
  data.frame(name = names, server = servers, stringsAsFactors = FALSE)
}

#' Connect User Account
#'
#' Connect a user account to the package so that it can be used to deploy and
#' manage applications on behalf of the account.
#'
#' @param account A name for the account to connect. Optional.
#' @param server The server to connect to. Optional if there is only one server
#'   registered.
#' @param quiet Whether or not to show messages and prompts while connecting the
#'   account.
#'
#' @details When this function is invoked, a web browser will be opened to a
#'   page on the target server where you will be prompted to enter your
#'   credentials. Upon successful authentication, your local installation of
#'   \pkg{rsconnect} and your server account will be paired, and you'll
#'   be able to deploy and manage applications using the package without further
#'   prompts for credentials.
#'
#' @export
connectUser <- function(account = NULL, server = NULL, quiet = FALSE) {
  # if server isn't specified, look up the default
  if (is.null(server)) {
    target <- getDefaultServer(local = TRUE)
  } else {
    target <- serverInfo(server)
  }

  if (is.null(target)) {
    stop("You must specify a server to connect to.")
  }

  # if account is specified and we already know about the account, get the User
  # ID so we can prefill authentication fields
  userId <- 0
  userAccounts <- accounts(target$name)
  if (!is.null(account) && !is.null(userAccounts)) {
    if (account %in% userAccounts[,"name"]) {
      accountDetails <- accountInfo(account, target$name)
      userId <- accountDetails$accountId
      if (!quiet) {
        message("The account '",  account, "' is already registered; ",
                "attempting to reconnect it.")
      }
    }
  }

  # generate a token and send it to the server
  token <- getAuthToken(target$name)
  if (!quiet) {
    message("A browser window should open; if it doesn't, you may authenticate ",
            "manually by visiting ", token$claim_url, ".")
    message("Waiting for authentication...")
  }
  utils::browseURL(token$claim_url)

  # keep trying to authenticate until we're successful
  repeat {
    Sys.sleep(1)
    user <- getUserFromRawToken(target$url, token$token, token$private_key)
    if (!is.null(user))
      break
  }

  # populate the username if there wasn't one set on the server
  if (nchar(user$username) == 0) {
    if (!is.null(account))
      user$username <- account
    else
      user$username <- tolower(paste0(substr(user$first_name, 1, 1),
                                      user$last_name))

    # in interactive mode, prompt for a username before accepting defaults
    if (!quiet && interactive() && is.null(account)) {
      input <- readline(paste0("Choose a nickname for this account (default '",
                               user$username, "'): "))
      if (nchar(input) > 0)
        user$username <- input
    }
  }

  # write the user info
  registerUserToken(serverName = target$name,
                    accountName = user$username,
                    userId = user$id,
                    token = token$token,
                    privateKey = token$private_key)

  if (!quiet) {
    message("Account registered successfully: ", user$first_name, " ",
            user$last_name, " (", user$username, ")")
  }
}

#' Set ShinyApps Account Info
#'
#' Configure a ShinyApps account for publishing from this system.
#'
#' @param name Name of account to save or remove
#' @param token User token for the account
#' @param secret User secret for the account
#'
#' @examples
#' \dontrun{
#'
#' # register an account
#' setAccountInfo("user", "token", "secret")
#'
#' # remove the same account
#' removeAccount("user")
#' }
#'
#' @export
setAccountInfo <- function(name, token, secret) {

  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))

  if (!isStringParam(token))
    stop(stringParamErrorMessage("token"))

  if (!isStringParam(secret))
    stop(stringParamErrorMessage("secret"))

  # create connect client
  authInfo <- list(token = token, secret = secret)
  serverInfo <- shinyappsServerInfo()
  lucid <- lucidClient(serverInfo$url, authInfo)

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
  configFile <- accountConfigFile(name, serverInfo$name)
  dir.create(dirname(configFile), recursive = TRUE, showWarnings = FALSE)

  # write the user info
  write.dcf(list(name = name,
                 userId = userId,
                 accountId = accountId,
                 token = token,
                 secret = secret,
                 server = serverInfo$name),
            configFile,
            width = 100)

  # set restrictive permissions on it if possible
  if (identical(.Platform$OS.type, "unix"))
    Sys.chmod(configFile, mode="0600")
}

#' @rdname accounts
#' @export
accountInfo <- function(name, server = NULL) {

  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))

  configFile <- accountConfigFile(name, server)
  if (length(configFile) > 1)
    stopWithMultipleAccounts(name)
  if (length(configFile) == 0 || !file.exists(configFile))
    stop(missingAccountErrorMessage(name))

  accountDcf <- readDcf(configFile, all = TRUE)
  info <- as.list(accountDcf)
  # remove all whitespace from private key
  if (!is.null(info$private_key)) {
    info$private_key <- gsub("[[:space:]]","",info$private_key)
  }
  info
}

#' @rdname accounts
#' @export
removeAccount <- function(name, server = NULL) {

  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))

  configFile <- accountConfigFile(name, server)
  if (!file.exists(configFile))
    stop(missingAccountErrorMessage(name))

  file.remove(configFile)

  invisible(NULL)
}

# given the name of a registered server, does the following:
# 1) generates a public/private key pair and token ID
# 2) pushes the public side of the key pair to the server, and obtains
#    from the server a URL at which the token can be claimed
# 3) returns the token ID, private key, and claim URL
getAuthToken <- function(server, userId = 0) {
  if (missing(server) || is.null(server)) {
    stop("You must specify a server to connect to.")
  }
  target <- serverInfo(server)

  # generate a token and push it to the server
  token <- generateToken()
  connect <- connectClient(service = target$url, authInfo = list())
  response <- connect$addToken(list(token = token$token,
                                    public_key = token$public_key,
                                    user_id = as.integer(userId)))

  # return the generated token and the information needed to claim it
  list(
    token = token$token,
    private_key = token$private_key,
    claim_url = response$token_claim_url)
}

# given a server URL and raw information about an auth token, return the user
# who owns the token, if it's claimed, and NULL if the token is unclaimed.
# raises an error on any other HTTP error.
getUserFromRawToken <- function(serverUrl, token, privateKey) {
  # form a temporary client from the raw token
  connect <- connectClient(service = serverUrl, authInfo =
                           list(token = token,
                                private_key = as.character(privateKey)))

  # attempt to fetch the user
  user <- NULL
  tryCatch({
    user <- connect$currentUser()
  }, error = function(e) {
    if (length(grep("HTTP 500", e$message)) == 0) {
      stop(e)
    }
  })

  # return the user we found
  user
}

registerUserToken <- function(serverName, accountName, userId, token,
                              privateKey) {
  # write the user info
  configFile <- accountConfigFile(accountName, serverName)
  dir.create(dirname(configFile), recursive = TRUE, showWarnings = FALSE)
  write.dcf(list(username = accountName,
                 accountId = userId,
                 token = token,
                 server = serverName,
                 private_key = as.character(privateKey)),
            configFile)

  # set restrictive permissions on it if possible
  if (identical(.Platform$OS.type, "unix"))
    Sys.chmod(configFile, mode="0600")
}

accountConfigFile <- function(name, server = NULL) {
  # if no server is specified, try to find an account with the given name
  # associated with any server
  if (is.null(server)) {
    return(list.files(accountsConfigDir(), pattern = paste0(name, ".dcf"),
                      recursive = TRUE, full.names = TRUE))
  }
  normalizePath(file.path(accountsConfigDir(), server,
                          paste(name, ".dcf", sep="")),
                mustWork = FALSE)
}

accountsConfigDir <- function() {
  rsconnectConfigDir("accounts")
}

missingAccountErrorMessage <- function(name) {
  paste("account named '", name, "' does not exist", sep="")
}

resolveAccount <- function(account, server = NULL) {

  # get existing accounts
  accounts <- accounts(server)[,"name"]
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
    count <- sum(accounts == account)
    if (count == 0)
      stopWithMissingAccount(account)
    else if (count == 1)
      account
    else
      stopWithMultipleAccounts(account)
  }
}

isShinyapps <- function(accountInfo) {
  identical(accountInfo$server, "shinyapps.io")
}

stopWithNoAccount <- function() {
  stop("You must register an account using setAccountInfo prior to ",
       "proceeding.", call. = FALSE)
}

stopWithSpecifyAccount <- function() {
  stop("Please specify the account name (there is more than one ",
       "account registered on this system)", call. = FALSE)
}

stopWithMissingAccount <- function(account) {
  stop(missingAccountErrorMessage(account), call. = FALSE)
}

stopWithMultipleAccounts <- function(account) {
  stop("Multiple accounts with the name '", account, "' exist. Please specify ",
       "the server of the account you wish to use.", call. = FALSE)
}
