#' Account Management Functions
#'
#' Functions to enumerate and remove accounts on the local system. Prior to
#' deploying applications you need to register your account on the local system.
#'
#' You register an account using the [setAccountInfo()] function (for
#' ShinyApps) or [connectUser()] function (for other servers). You can
#' subsequently remove the account using the `removeAccount` function.
#'
#' The `accounts` and `accountInfo` functions are provided for viewing
#' previously registered accounts.
#'
#' @param name Name of account
#' @param server Name of the server on which the account is registered
#'   (optional; see [servers()])
#'
#' @return `accounts` returns a data frame with the names of all accounts
#' registered on the system and the servers on which they reside.
#' `accountInfo` returns a list with account details.
#'
#' @rdname accounts
#' @export
accounts <- function(server = NULL) {
  path <- accountsConfigDir()
  if (!is.null(server))
    path <- file.path(path, server)

  # get a raw list of accounts
  accountnames <- file_path_sans_ext(list.files(path,
    pattern = glob2rx("*.dcf"), recursive = TRUE, full.names = TRUE))

  if (length(accountnames) == 0) {
    return(NULL)
  }

  # convert to a data frame
  servers <- dirname(accountnames)
  servers[servers == "."] <- "shinyapps.io"
  servers <- fileLeaf(servers)
  names <- fileLeaf(accountnames)
  data.frame(name = names, server = servers, stringsAsFactors = FALSE)
}

#' Connect Api User Account
#'
#' Connect a user account to the package using an API key for authentication
#' so that it can be used to deploy and manage
#' applications on behalf of the account.
#'
#' @param account A name for the account to connect. Optional.
#' @param server The server to connect to. Optional if there is only one server
#'   registered.
#' @param apiKey The API key used to authenticate the user
#' @param quiet Whether or not to show messages and prompts while connecting the
#'   account.
#'
#' @details This function configures the user to connect using an apiKey in
#' the http auth headers instead of a token. This is less secure but may
#' be necessary when the client is behind a proxy or otherwise unable to
#' authenticate using a token.
#'
#' @family Account functions
#' @export
connectApiUser <- function(account = NULL, server = NULL, apiKey = NULL, quiet = FALSE) {
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
    if (account %in% userAccounts[, "name"]) {
      accountDetails <- accountInfo(account, target$name)
      userId <- accountDetails$accountId
      if (!quiet) {
        message("The account '",  account, "' is already registered; ",
                "attempting to reconnect it.")
      }
    }
  }

  user <- getAuthedUser(serverUrl = target$url,
                        apiKey = apiKey)

  if (is.null(user)) {
    stop("Unable to fetch user data for provided API key")
  }

  # write the user info
  registerUserApiKey(serverName = target$name,
                    accountName = account,
                    userId = user$id,
                    apiKey = apiKey)

  if (!quiet) {
    message("\nAccount registered successfully: ", account)
  }
}

#' Connect User Account
#'
#' Connect a user account to the package so that it can be used to deploy and
#' manage applications on behalf of the account.
#'
#' @param account A name for the account to connect. Optional.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to `TRUE` in
#'   interactive sessions only. If a function is passed, it will be called
#'   after the app is started, with the app URL as a paramter.
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
#' @family Account functions
#' @export
connectUser <- function(account = NULL, server = NULL, quiet = FALSE,
                        launch.browser = getOption("rsconnect.launch.browser", interactive())) {
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
    if (account %in% userAccounts[, "name"]) {
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

  if (isTRUE(launch.browser))
    utils::browseURL(token$claim_url)
  else if (is.function(launch.browser))
    launch.browser(token$claim_url)

  # keep trying to authenticate until we're successful
  repeat {
    Sys.sleep(1)
    user <- getAuthedUser(serverUrl = target$url,
                          token = token$token,
                          privateKey = token$private_key,
                          serverCertificate = target$certificate)
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

#' Set ShinyApps or Posit Cloud Account Info
#'
#' Configure a ShinyApps or Posit Cloud account for publishing from this system.
#'
#' @param name Name of account to save or remove
#' @param token User token for the account
#' @param secret User secret for the account
#' @param server Server to associate account with.
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
#' @family Account functions
#' @export
setAccountInfo <- function(name, token, secret,
                           server = "shinyapps.io") {

  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))

  if (!isStringParam(token))
    stop(stringParamErrorMessage("token"))

  if (!isStringParam(secret))
    stop(stringParamErrorMessage("secret"))

  # create connect client
  if (identical(server, cloudServerInfo(server)$name)) {
    serverInfo <- cloudServerInfo(server)
  } else {
    serverInfo <- shinyappsServerInfo()
  }
  authInfo <- list(token = token,
                   secret = secret,
                   certificate = serverInfo$certificate,
                   server = serverInfo$name)
  lucid <- lucidClientForAccount(authInfo)

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
    Sys.chmod(configFile, mode = "0600")
}

#' @rdname accounts
#' @family Account functions
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
    info$private_key <- gsub("[[:space:]]", "", info$private_key)
  }
  info
}

#' @rdname accounts
#' @export
removeAccount <- function(name, server = NULL) {

  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))

  configFile <- accountConfigFile(name, server)
  if (length(configFile) > 1)
    stopWithMultipleAccounts(name)
  if (length(configFile) == 0 || !file.exists(configFile))
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
  connect <- connectClient(service = target$url,
                           authInfo = list(certificate = target$certificate))
  response <- connect$addToken(list(token = token$token,
                                    public_key = token$public_key,
                                    user_id = as.integer(userId)))

  # return the generated token and the information needed to claim it
  list(
    token = token$token,
    private_key = token$private_key,
    claim_url = response$token_claim_url)
}

# given a server URL and auth parameters, return the user
# who owns the auth if it's valid/claimed, and NULL if invalid/unclaimed.
# raises an error on any other HTTP error.
#
# this function is used by the RStudio IDE as part of the workflow which
# attaches a new Connect account.
getAuthedUser <- function(serverUrl,
                          token = NULL,
                          privateKey = NULL,
                          serverCertificate = NULL,
                          apiKey = NULL) {
  authInfo <- NULL
  if (!is.null(apiKey)) {
    authInfo <- list(apiKey = apiKey)
  } else {
    authInfo <- list(token = token,
                     private_key = as.character(privateKey),
                     certificate = serverCertificate)
  }

  # form a temporary client from the authInfo
  connect <- connectClient(service = ensureConnectServerUrl(serverUrl), authInfo)

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

# passthrough function for compatibility with old IDE versions
getUserFromRawToken <- function(serverUrl,
                                token,
                                privateKey,
                                serverCertificate = NULL) {
  getAuthedUser(serverUrl = serverUrl,
                token = token,
                privateKey = privateKey,
                serverCertificate = serverCertificate)
}

registerUserApiKey <- function(serverName, accountName, userId, apiKey) {
  # write the user info
  configFile <- accountConfigFile(accountName, serverName)
  dir.create(dirname(configFile), recursive = TRUE, showWarnings = FALSE)
  write.dcf(list(username = accountName,
                 accountId = userId,
                 apiKey = apiKey,
                 server = serverName),
            configFile)

  # set restrictive permissions on it if possible
  if (identical(.Platform$OS.type, "unix"))
    Sys.chmod(configFile, mode = "0600")
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
    Sys.chmod(configFile, mode = "0600")
}

accountConfigFile <- function(name, server = NULL) {
  # if no server is specified, try to find an account with the given name
  # associated with any server
  if (is.null(server)) {
    pat <- escapeRegex(paste0(name, ".dcf"))
    return(normalizePath(list.files(accountsConfigDir(), pattern = pat,
                                    recursive = TRUE, full.names = TRUE)))
  }
  normalizePath(file.path(accountsConfigDir(), server,
                          paste(name, ".dcf", sep = "")),
                mustWork = FALSE)
}

accountsConfigDir <- function() {
  rsconnectConfigDir("accounts")
}

missingAccountErrorMessage <- function(name) {
  paste("account named '", name, "' does not exist", sep = "")
}

resolveAccount <- function(account, server = NULL) {

  # get existing accounts
  accounts <- accounts(server)[, "name"]
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

isCloudServer <- function(server) {
  identical(server, "shinyapps.io") ||
    identical(server, cloudServerInfo(server)$name)
}

isShinyappsServer <- function(server) {
  identical(server, "shinyapps.io")
}

isRPubs <- function(server) {
  identical(server, "rpubs.com")
}

isConnectInfo <- function(accountInfo = NULL, server = NULL) {
  host <- if (is.null(accountInfo)) server else accountInfo$server
  !isCloudServer(host) && !isRPubs(host)
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

accountInfoFromHostUrl <- function(hostUrl) {
  # get the list of all registered servers
  servers <- servers()

  # filter to just those matching the given host url
  server <- servers[as.character(servers$url) == hostUrl, ]
  if (nrow(server) < 1) {
    stop("No server with the URL ", hostUrl, " is registered.", call. = FALSE)
  }

  # extract server name
  server <- as.character(server[1, "name"])

  # now find accounts with the given server
  account <- accounts(server = server)
  if (is.null(account) || nrow(account) < 1) {
    stop("No accounts registered with server ", server, call. = FALSE)
  }

  # return account info from the first one
  return(accountInfo(name = as.character(account[1, "name"]),
                     server = server))
}
