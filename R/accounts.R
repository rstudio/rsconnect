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
  configPaths <- accountConfigFiles(server)

  names <- file_path_sans_ext(basename(configPaths))

  servers <- basename(dirname(configPaths))
  servers[servers == "."] <- "shinyapps.io"

  data.frame(name = names, server = servers, stringsAsFactors = FALSE)
}

#' Connect user account
#
#' @description
#' `connectUser()` and `connectApiUser()` connect your Posit Connect account to
#' the rsconnect package so that it can deploy and manage applications on
#' your behalf. `connectApiUser()` uses an API key sent in the HTTP headers,
#' instead of a token. This is less secure but may be necessary when the client
#' is behind a proxy or otherwise unable to authenticate using a token.
#'
#' `connectUser()` needs to be run in an interactive session because it
#' requires you to interactively authenticate to the Posit Connect server.
#' `connectApiUser()` requires an API key copy-and-pasted from the API keys
#' found in your account settings.
#'
#' @param account A name for the account to connect.
#' @param server The server to connect to.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to `TRUE` in
#'   interactive sessions only. If a function is passed, it will be called
#'   after the app is started, with the app URL as a parameter.
#' @param apiKey The API key used to authenticate the user
#' @param quiet Whether or not to show messages and prompts while connecting the
#'   account.
#' @family Account functions
#' @export
connectApiUser <- function(account = NULL, server = NULL, apiKey = NULL, quiet = FALSE) {
  server <- findServer(server)
  target <- serverInfo(server)

  user <- getAuthedUser(serverUrl = target$url, apiKey = apiKey)
  if (is.null(user)) {
    stop("Unable to fetch user data for provided API key")
  }

  registerUserApiKey(
    serverName = target$name,
    accountName = account,
    userId = user$id,
    apiKey = apiKey
  )

  if (!quiet) {
    message("\nAccount registered successfully: ", account)
  }
}

#' @rdname connectApiUser
#' @export
connectUser <- function(account = NULL, server = NULL, quiet = FALSE,
                        launch.browser = getOption("rsconnect.launch.browser", interactive())) {
  server <- findServer(server)
  target <- serverInfo(server)

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

  registerUserToken(
    serverName = target$name,
    accountName = user$username,
    userId = user$id,
    token = token$token,
    privateKey = token$private_key
  )

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
setAccountInfo <- function(name, token, secret, server = "shinyapps.io") {
  check_string(name)
  check_string(token)
  check_string(secret)
  check_string(server)

  account <- list(token = token, secret = secret, server = server)
  client <- clientForAccount(account)

  # get user Id
  userId <- client$currentUser()$id

  # get account id
  accountId <- NULL
  accounts <- client$accountsForUser(userId)
  for (account in accounts) {
    if (identical(account$name, name)) {
      accountId <- account$id
      break
    }
  }
  if (is.null(accountId))
    stop("Unable to determine account id for account named '", name, "'")

  registerCloudTokenSecret(
    serverName = serverInfo$name,
    accountName = name,
    userId = userId,
    token = token,
    secret = secret,
  )
  invisible()
}

registerCloudTokenSecret <- function(serverName,
                                     accountName,
                                     userId,
                                     accountId,
                                     token,
                                     secret) {
  # get the path to the config file
  path <- accountConfigFile(accountName, serverName)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  # write the user info
  write.dcf(
    list(
      name = accountName,
      userId = userId,
      accountId = accountName,
      token = token,
      secret = secret,
      server = serverName
    ),
    path,
    width = 100
  )

  # set restrictive permissions on it if possible
  if (identical(.Platform$OS.type, "unix"))
    Sys.chmod(path, mode = "0600")

  path
}

#' @rdname accounts
#' @family Account functions
#' @export
accountInfo <- function(name = NULL, server = NULL) {

  fullAccount <- findAccount(name, server)
  configFile <- accountConfigFile(fullAccount$name, fullAccount$server)

  accountDcf <- readDcf(configFile, all = TRUE)
  info <- as.list(accountDcf)
  # remove all whitespace from private key
  if (!is.null(info$private_key)) {
    info$private_key <- secret(gsub("[[:space:]]", "", info$private_key))
  }

  if (!is.null(info$secret)) {
    info$secret <- secret(info$secret)
  }

  info
}

#' @rdname accounts
#' @export
removeAccount <- function(name = NULL, server = NULL) {
  fullAccount <- findAccount(name, server)

  configFile <- accountConfigFile(name, server)
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

missingAccountErrorMessage <- function(name) {
  paste("account named '", name, "' does not exist", sep = "")
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


secret <- function(x) {
  if (is.null(x)) return(NULL)

  stopifnot(is.character(x) || all(is.na(x)))
  structure(x, class = "rsconnect_secret")
}

#' @export
format.rsconnect_secret <- function(x, ...) {
  paste0(substr(x, 1, 6), "... (redacted)")
}

#' @export
print.rsconnect_secret <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
str.rsconnect_secret <- function(object, ...) {
  cat(" ", format(object), "\n", sep = "")
}

accountId <- function(account, server) {
  paste0(account, "@", server)
}
