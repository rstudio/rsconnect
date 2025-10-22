#' Account Management Functions
#'
#' @description
#' Functions to enumerate and remove accounts on the local system. Prior to
#' deploying applications you need to register your account on the local system.
#'
#' Supported servers: All servers
#'
#' @details
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
#' Supported servers: Posit Connect servers
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
connectApiUser <- function(
  account = NULL,
  server = NULL,
  apiKey,
  quiet = FALSE
) {
  server <- findServer(server)
  checkConnectServer(server)

  user <- getAuthedUser(server, apiKey = apiKey)

  registerAccount(
    serverName = server,
    accountName = account %||% user$username,
    accountId = user$id,
    apiKey = apiKey
  )

  if (!quiet) {
    accountLabel <- accountLabel(user$username, server)
    cli::cli_alert_success("Registered account for {accountLabel}")
  }
  invisible()
}


#' Register account on Posit Connect in Snowpark Container Services
#'
#' @description
#' `connectSPCSUser()` connects your Posit Connect account to the rsconnect
#' package so it can deploy and manage applications on your behalf.
#' Configure a
#' [`connections.toml` file](https://docs.snowflake.com/en/developer-guide/snowflake-cli/connecting/configure-cli#location-of-the-toml-configuration-fil)
#' in the appropriate location.
#'
#' Supported servers: Posit Connect servers
#'
#' @inheritParams connectApiUser
#' @param snowflakeConnectionName Name for the Snowflake connection parameters
#'   stored in `connections.toml`.
#' @export
connectSPCSUser <- function(
  account = NULL,
  server = NULL,
  snowflakeConnectionName,
  quiet = FALSE
) {
  server <- findServer(server)
  checkConnectServer(server)

  user <- getSPCSAuthedUser(server, snowflakeConnectionName)

  registerAccount(
    serverName = server,
    accountName = account %||% user$username,
    accountId = user$id,
    snowflakeConnectionName = snowflakeConnectionName
  )

  if (!quiet) {
    accountLabel <- accountLabel(user$username, server)
    cli::cli_alert_success("Registered account for {accountLabel}")
  }

  invisible()
}

getSPCSAuthedUser <- function(server, snowflakeConnectionName) {
  serverAddress <- serverInfo(server)
  account <- list(
    server = server,
    snowflakeConnectionName = snowflakeConnectionName
  )

  client <- clientForAccount(account)
  client$currentUser()
}

#' @rdname connectApiUser
#' @export
connectUser <- function(
  account = NULL,
  server = NULL,
  quiet = FALSE,
  launch.browser = getOption("rsconnect.launch.browser", interactive())
) {
  server <- findServer(server)
  checkConnectServer(server)

  resp <- getAuthTokenAndUser(server, launch.browser)

  registerAccount(
    serverName = server,
    accountName = account %||% resp$user$username,
    accountId = resp$user$id,
    token = resp$token$token,
    private_key = resp$token$private_key
  )

  if (!quiet) {
    accountLabel <- accountLabel(resp$user$username, server)
    cli::cli_alert_success("Registered account for {accountLabel}")
  }
  invisible()
}

# Filter accounts to those where the user has permission to create (i.e. publish) content.
filterPublishableAccounts <- function(accounts) {
  Filter(
    function(account) {
      any(vapply(
        account$permissions,
        function(permission) {
          identical(permission, "content:create")
        },
        logical(1)
      ))
    },
    accounts
  )
}

#' Register account on Posit Connect Cloud
#
#' @description
#' `connectCloudUser()` connects your Posit Connect Cloud account to
#' the rsconnect package so that it can deploy and manage applications on
#' your behalf. It will open a browser window to authenticate, then prompt
#' you to create an account or select an account to use if you have multiple.
#'
#' Supported servers: Posit Connect Cloud servers
#'
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to `TRUE` in
#'   interactive sessions only. If a function is passed, it will be called
#'   after the app is started, with the app URL as a parameter.
#'
#' @family Account functions
#' @export
connectCloudUser <- function(launch.browser = TRUE) {
  authClient <- cloudAuthClient()
  deviceAuth <- authClient$createDeviceAuth()

  verificationUriComplete <- addUtmParameters(
    deviceAuth$verification_uri_complete
  )

  # Alert user and open browser for verification
  if (isTRUE(launch.browser)) {
    cli::cli_alert_info(
      "Opening login page - confirm the code entered matches the code: {deviceAuth$user_code}."
    )
    utils::browseURL(verificationUriComplete)
  } else if (is.function(launch.browser)) {
    cli::cli_alert_info(
      "Opening login page - confirm the code entered matches the code: {deviceAuth$user_code}."
    )
    launch.browser(verificationUriComplete)
  } else {
    cli::cli_alert_info(
      "Open {.url {verificationUriComplete}} to authenticate and confirm the code entered matches the code: {deviceAuth$user_code}."
    )
  }

  tokenResponse <- waitForDeviceAuth(authClient, deviceAuth)
  accessToken <- tokenResponse$access_token
  refreshToken <- tokenResponse$refresh_token

  client <- connectCloudClient(
    parseHttpUrl(connectCloudUrls()$api),
    list(accessToken = accessToken, refreshToken = refreshToken)
  )

  getAccounts <- function() {
    accountsResponse <- tryCatch(
      {
        response <- client$getAccounts()
        response$data
      },
      rsconnect_http_401 = function(err) {
        if (err$errorType == "no_user_for_lucid_user") {
          return(list())
        }
        stop(err)
      }
    )
  }

  accounts <- getAccounts()
  accountsWhereUserCanPublish <- filterPublishableAccounts(accounts)
  cloudUiUrl <- connectCloudUrls()$ui
  if (length(accountsWhereUserCanPublish) == 0) {
    if (length(accounts) == 0) {
      accountCreationPage <- addUtmParameters(paste0(
        cloudUiUrl,
        "/account/done"
      ))
      if (isTRUE(launch.browser)) {
        cli::cli_alert_info(
          "To deploy, you must first create an account. Opening account creation page..."
        )
        utils::browseURL(accountCreationPage)
      } else if (is.function(launch.browser)) {
        cli::cli_alert_info(
          "To deploy, you must first create an account. Opening account creation page..."
        )
        launch.browser(accountCreationPage)
      } else {
        cli::cli_alert_info(
          "To deploy, you must first create an account. Please go to {.url accountCreationPage} to create one."
        )
      }
      # poll for account for up to 10 minutes
      for (i in 1:300) {
        Sys.sleep(2)
        accounts <- getAccounts()
        if (length(accounts) > 0) {
          accountsWhereUserCanPublish <- filterPublishableAccounts(accounts)
          if (length(accountsWhereUserCanPublish) > 0) {
            break
          }
        }
      }
      if (length(accountsWhereUserCanPublish) == 0) {
        cli::cli_abort(
          "Timed out waiting for an account to be created. Try again after creating a new account."
        )
      }
    } else {
      cli::cli_abort(
        "You do not have permission to publish content on any of your accounts. To publish, you may create a new account at {.url cloudUiUrl}."
      )
    }
  }

  # prompt the user to select an account if there's more than one they can publish to
  if (length(accountsWhereUserCanPublish) > 1) {
    cli::cli_alert_info("You have permission to publish to multiple accounts.")
    accountNames <- vapply(
      accountsWhereUserCanPublish,
      function(account) account$name,
      character(1)
    )
    # selected <- utils::menu(accountNames, title = "Select an account to use:")
    selected <- cli_menu(
      "Multiple accounts found.",
      "Which account do you want to use?",
      accountNames
    )
    account <- accountsWhereUserCanPublish[[selected]]
  } else {
    account <- accountsWhereUserCanPublish[[1]]
  }

  registerAccount(
    serverName = "connect.posit.cloud",
    accountName = account$name,
    accountId = account$id,
    accessToken = accessToken,
    refreshToken = refreshToken
  )

  cli::cli_alert_success("Registered account.")
}

# Poll the server until the user has completed device authentication, returning
# the token response once finished.
waitForDeviceAuth <- function(authClient, deviceAuth) {
  pollingInterval <- deviceAuth$interval
  while (TRUE) {
    Sys.sleep(pollingInterval)
    tokenResponse <- tryCatch(
      authClient$exchangeToken(list(
        grant_type = "urn:ietf:params:oauth:grant-type:device_code",
        device_code = deviceAuth$device_code
      )),
      rsconnect_http_400 = function(err) {
        errorCode <- err$body
        if (errorCode == "authorization_pending") {
          return(NULL)
        } else if (errorCode == "slow_down") {
          pollingInterval <<- pollingInterval + 5
          return(NULL)
        } else if (errorCode == "expired_token") {
          cli::cli_abort("Verification code has expired.")
        } else if (errorCode == "access_denied") {
          cli::cli_abort("Authorization request was denied.")
        }
        cli::cli_abort("Error during authentication: {error_code}")
      }
    )
    if (!is.null(tokenResponse)) {
      return(tokenResponse)
    }
  }
}


getAuthTokenAndUser <- function(server, launch.browser = TRUE) {
  token <- getAuthToken(server)

  if (isTRUE(launch.browser)) {
    utils::browseURL(token$claim_url)
  } else if (is.function(launch.browser)) {
    launch.browser(token$claim_url)
  }

  if (isFALSE(launch.browser)) {
    cli::cli_alert_warning("Open {.url {token$claim_url}} to authenticate")
  } else {
    cli::cli_alert_info(
      "A browser window should open to complete authentication"
    )
    cli::cli_alert_warning(
      "If it doesn't open, please go to {.url {token$claim_url}}"
    )
  }

  user <- waitForAuthedUser(
    server,
    token = token$token,
    private_key = token$private_key
  )

  list(
    token = token,
    user = user
  )
}

# Used by the IDE
getAuthToken <- function(server, userId = 0) {
  token <- generateToken()

  # Send public key to server, and generate URL where the token can be claimed
  account <- list(server = server)
  client <- clientForAccount(account)
  response <- client$addToken(list(
    token = token$token,
    public_key = token$public_key,
    user_id = 0L
  ))

  list(
    token = token$token,
    private_key = secret(token$private_key),
    claim_url = response$token_claim_url
  )
}

# generateToken generates a token for signing requests sent to the Posit
# Connect service. The token's ID and public key are sent to the server, and
# the private key is saved locally.
generateToken <- function() {
  key <- openssl::rsa_keygen(2048L)
  priv.der <- openssl::write_der(key)
  pub.der <- openssl::write_der(key$pubkey)
  tokenId <- paste(c("T", openssl::rand_bytes(16)), collapse = "")

  list(
    token = tokenId,
    public_key = openssl::base64_encode(pub.der),
    private_key = openssl::base64_encode(priv.der)
  )
}

waitForAuthedUser <- function(
  server,
  token = NULL,
  private_key = NULL,
  apiKey = NULL
) {
  # keep trying to authenticate until we're successful; server returns
  # 500 "Token is unclaimed error" (Connect before 2024.05.0)
  # 401 "Unauthorized" occurs before the token has been claimed.
  cli::cli_progress_bar(format = "{cli::pb_spin} Waiting for authentication...")

  repeat {
    for (i in 1:10) {
      Sys.sleep(0.1)
      cli::cli_progress_update()
    }
    user <- tryCatch(
      getAuthedUser(
        server,
        token = token,
        private_key = private_key,
        apiKey = apiKey
      ),
      rsconnect_http_401 = function(err) NULL,
      rsconnect_http_500 = function(err) NULL
    )
    if (!is.null(user)) {
      cli::cli_progress_done()
      break
    }
  }

  user
}

getAuthedUser <- function(
  server,
  token = NULL,
  private_key = NULL,
  apiKey = NULL
) {
  if (!xor(is.null(token) && is.null(private_key), is.null(apiKey))) {
    cli::cli_abort(
      "Must supply either {.arg token} + {private_key} or {.arg apiKey}"
    )
  }

  account <- list(
    server = server,
    apiKey = apiKey,
    token = token,
    private_key = private_key
  )
  client <- clientForAccount(account)
  client$currentUser()
}

#' Register account on shinyapps.io
#'
#' @description
#' Configure a ShinyApps account for publishing from this system.
#'
#' Supported servers: ShinyApps servers
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
findShinyAppsAccountId <- function(
  name,
  token,
  secret,
  server,
  error_call = caller_env()
) {
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
  cli::cli_abort(
    "Unable to determine {.arg accountId} for account {.str {name}}"
  )
}

#' @rdname accounts
#' @family Account functions
#' @export
accountInfo <- function(name = NULL, server = NULL) {
  findAccountInfo(name, server)
}

# Discovers then loads details about an account from disk.
# Internal equivalent to accountInfo that lets callers provide error context.
findAccountInfo <- function(
  name = NULL,
  server = NULL,
  error_call = caller_env()
) {
  fullAccount <- findAccount(name, server, error_call = error_call)
  configFile <- accountConfigFile(fullAccount$name, fullAccount$server)

  accountDcf <- read.dcf(configFile, all = TRUE)
  info <- as.list(accountDcf)

  # Account records previously had username, now have name. Internal callers expect "name", but
  # external callers may expect "username". (#1024)
  info$name <- info$name %||% info$username
  info$username <- info$name

  # remove all whitespace from private key
  if (!is.null(info$private_key)) {
    info$private_key <- gsub("[[:space:]]", "", info$private_key)
  }

  # Hide credentials
  info$private_key <- secret(info$private_key)
  info$secret <- secret(info$secret)
  info$apiKey <- secret(info$apiKey)
  info$snowflakeToken <- secret(info$snowflakeToken)

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

registerAccount <- function(
  serverName,
  accountName,
  accountId,
  token = NULL,
  secret = NULL,
  private_key = NULL,
  apiKey = NULL,
  snowflakeConnectionName = NULL,
  accessToken = NULL,
  refreshToken = NULL
) {
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
    apiKey = apiKey,
    snowflakeConnectionName = snowflakeConnectionName,
    accessToken = accessToken,
    refreshToken = refreshToken
  )

  path <- accountConfigFile(accountName, serverName)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.dcf(compact(fields), path, width = 100)

  # set restrictive permissions on it if possible
  if (identical(.Platform$OS.type, "unix")) {
    Sys.chmod(path, mode = "0600")
  }

  path
}

accountLabel <- function(account, server) {
  # Note: The incoming "account" may correspond to our local account name, which does not always
  # match the remote username.
  paste0("server: ", server, " / username: ", account)
}
