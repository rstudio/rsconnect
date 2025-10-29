# These functions are intended to be called primarily by the RStudio IDE.

# This function is poorly named because as well as validating the server
# url it will also register the server if needed.
validateServerUrl <- function(url, certificate = NULL) {
  res <- validateConnectUrl(url, certificate)

  if (res$valid) {
    name <- findAndRegisterLocalServer(res$url)
    c(list(valid = TRUE, url = res$url, name = name), res$response)
  } else {
    res
  }
}

# given a server URL, returns that server's short name. if the server is not
# currently registered, the server is registered and the short name of the newly
# registered server is returned.
findAndRegisterLocalServer <- function(url) {
  # helper to find a server given its URL
  findServerByUrl <- function(url) {
    allServers <- rsconnect::servers(local = TRUE)
    match <- allServers[allServers$url == url, , drop = FALSE]
    if (nrow(match) == 0) {
      NULL
    } else {
      as.character(match[1, "name"])
    }
  }

  # if there are no local servers with the given URL, add one and return its
  # name
  name <- findServerByUrl(url)
  if (is.null(name)) {
    url <- ensureConnectServerUrl(url)
    addServer(
      url = url,
      name = NULL,
      certificate = NULL,
      quiet = TRUE,
      validate = FALSE
    )
    findServerByUrl(url)
  } else {
    name
  }
}

# Called directly by RStudio.
# See https://github.com/rstudio/rstudio/blob/main/src/cpp/session/modules/SessionRSConnect.R
registerUserToken <- function(
  serverName,
  accountName,
  userId,
  token,
  privateKey,
  accessToken = NULL,
  refreshToken = NULL
) {
  registerAccount(
    serverName = serverName,
    accountName = accountName,
    accountId = userId,
    token = token,
    private_key = privateKey,
    accessToken = accessToken,
    refreshToken = refreshToken
  )
}

# generate the markers
showRstudioSourceMarkers <- function(basePath, lint) {
  markers <- list()
  applied <- lapply(lint, function(file) {
    lapply(file, function(linter) {
      lapply(linter$indices, function(index) {
        marker <- list()
        marker$type <- "warning"
        marker$file <- file.path(basePath, linter$file)
        marker$line <- index
        marker$column <- 1
        marker$message <- linter$suggestion
        markers <<- c(markers, list(marker))
        marker
      })
    })
  })

  rstudioapi::callFun(
    "sourceMarkers",
    name = "Publish Content Issues",
    markers = markers,
    basePath = basePath,
    autoSelect = "first"
  )
}

# getAppById() -----------------------------------------------------------------

# https://github.com/rstudio/rstudio/blob/ee56d49b0fca5f3d7c3f5214a4010355d1bb0212/src/gwt/src/org/rstudio/studio/client/rsconnect/ui/RSConnectDeploy.java#L699

getAppById <- function(id, account, server, hostUrl) {
  check_string(account)
  check_string(server)
  check_string(hostUrl)

  if (!hasAccount(account, server)) {
    # If can't find record for account + server, try hostUrl
    servers <- servers()
    matches <- servers$url == hostUrl
    if (any(matches)) {
      server <- servers$name[which(matches)[[1]]]
      if (!hasAccount(account, server)) {
        cli::cli_abort(
          "Can't find account {.str {account}} on server {.str {server}}."
        )
      }
    } else {
      cli::cli_abort("Can't find server with url {.str {hostUrl}}.")
    }
  }

  getApplication(account, server, id)
}


# -------------------------------------------------------------------------

# passthrough function for compatibility with old IDE versions
getUserFromRawToken <- function(
  serverUrl,
  token,
  privateKey,
  serverCertificate = NULL
) {
  # Look up server name from url
  servers <- servers()
  matches <- servers$url == serverUrl
  server <- servers$name[which(matches)[[1]]]

  waitForAuthedUser(server, token = token, private_key = privateKey)
}
