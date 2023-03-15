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

#' Register account on Posit Connect
#
#' @description
#' `connectUser()` and `connectApiUser()` connect your Posit Connect account to
#' the rsconnect package so that it can deploy and manage applications on
#' your behalf.
#'
#' `connectUser()` is the easiest place to start because it allows you to
#' authenticate in-browser to your Posit Connect server. `connectApiUser()` is
#' appropriate for non-interactive settings; you'll need to copy-and-paste the
#' API key from your account settings.
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
connectApiUser <- function(account = NULL, server = NULL, apiKey, quiet = FALSE) {
  server <- findServer(server)
  user <- getAuthedUser(server, apiKey = apiKey)

  registerAccount(
    serverName = server,
    accountName = account %||% user$username,
    accountId = user$id,
    apiKey = apiKey
  )

  if (!quiet) {
    message("Account registered successfully: ", account %||% user$username)
  }
  invisible()
}

#' @rdname connectApiUser
#' @export
connectUser <- function(account = NULL, server = NULL, quiet = FALSE,
                        launch.browser = getOption("rsconnect.launch.browser", interactive())) {
  server <- findServer(server)

  # generate a token and send it to the server
  token <- getAuthToken(server)
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
    user <- getAuthedUser(server, token = token)
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

  registerAccount(
    serverName = server,
    accountName = user$username,
    accountId = user$id,
    token = token$token,
    private_key = token$private_key
  )

  if (!quiet) {
    message("Account registered successfully: ", user$first_name, " ",
            user$last_name, " (", user$username, ")")
  }
  invisible()
}

#' Register account on shinyapps.io or posit.cloud
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

  accountId <- findShinyAppsAccountId(name, token, secret, server)

  registerAccount(
    serverName = server,
    accountName = name,
    accountId = accountId,
    token = token,
    secret = secret
  )
  invisible()
}

# A user can have multiple accounts, so iterate over all accounts looking
# for one with the specified name
findShinyAppsAccountId <- function(name,
                                   token,
                                   secret,
                                   server,
                                   error_call = caller_env()) {
  if (secret == "<SECRET>") {
    cli::cli_abort(
      c(
        "You've copied and pasted the wrong thing.",
        i = "Either click 'Show secret' or 'Copy to clipboard'."
      ),
      call = error_call
    )
  }

  account <- list(token = token, secret = secret, server = server)
  client <- clientForAccount(account)

  userId <- client$currentUser()$id

  accountId <- NULL
  accounts <- client$accountsForUser(userId)
  for (account in accounts) {
    if (identical(account$name, name)) {
      return(account$id)
    }
  }
  cli::cli_abort("Unable to determine {.arg accountId} for account {.str {name}}")
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
    info$private_key <- gsub("[[:space:]]", "", info$private_key)
  }

  # Hide credentials
  info$private_key <- secret(info$private_key)
  info$secret <- secret(info$secret)
  info$apiKey <- secret(info$apiKey)

  info
}

hasAccount <- function(name, server) {
  file.exists(accountConfigFile(name, server))
}

#' @rdname accounts
#' @export
removeAccount <- function(name = NULL, server = NULL) {
  fullAccount <- findAccount(name, server)

  configFile <- accountConfigFile(fullAccount$name, fullAccount$server)
  file.remove(configFile)

  invisible(NULL)
}

# given the name of a registered server, does the following:
# 1) generates a public/private key pair and token ID
# 2) pushes the public side of the key pair to the server, and obtains
#    from the server a URL at which the token can be claimed
# 3) returns the token ID, private key, and claim URL
getAuthToken <- function(server, userId = 0) {
  # generate a token and push it to the server
  token <- generateToken()
  client <- clientForAccount(list(server = server))
  response <- client$addToken(list(
    token = token$token,
    public_key = token$public_key,
    user_id = as.integer(userId)
  ))

  # return the generated token and the information needed to claim it
  list(
    token = token$token,
    private_key = secret(token$private_key),
    claim_url = response$token_claim_url
  )
}

# given a server URL and auth parameters, return the user
# who owns the auth if it's valid/claimed, and NULL if invalid/unclaimed.
# raises an error on any other HTTP error.
#
# this function is used by the RStudio IDE as part of the workflow which
# attaches a new Connect account.
getAuthedUser <- function(server, token = NULL, apiKey = NULL) {
  if (!xor(is.null(token), is.null(apiKey))) {
    cli::cli_abort("Must supply exactly one of {.arg token} and {.arg apiKey}")
  }

  account <- list(server = server)
  if (!is.null(apiKey)) {
    account$apiKey <- apiKey
  } else {
    account$token <- token$token
    account$private_key <- token$private_key
  }
  client <- clientForAccount(account)

  # server returns 500 "Token is unclaimed error" while waiting for
  # interactive auth to complete
  tryCatch(
    client$currentUser(),
    rsconnect_http_500 = function(err) NULL
  )
}

registerAccount <- function(serverName,
                            accountName,
                            accountId,
                            token = NULL,
                            secret = NULL,
                            private_key = NULL,
                            apiKey = NULL) {

  check_string(serverName)
  check_string(accountName)
  if (!is.null(secret)) {
    secret <- as.character(secret)
  }

  fields <- list(
    name = accountName,
    server = serverName,
    accountId = accountId,
    token = token,
    secret = secret,
    private_key = private_key,
    apiKey = apiKey
  )

  path <- accountConfigFile(accountName, serverName)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.dcf(compact(fields), path, width = 100)

  # set restrictive permissions on it if possible
  if (identical(.Platform$OS.type, "unix"))
    Sys.chmod(path, mode = "0600")

  path
}

accountId <- function(account, server) {
  paste0(account, "@", server)
}
