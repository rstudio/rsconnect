# Returns the OAuth client ID to use for the configured Connect Cloud environment.
getClientId <- function() {
  switch(
    connectCloudEnvironment(),
    production = "rsconnect",
    staging = "rsconnect-staging",
    development = "rsconnect-development",
  )
}

# Creates a client for interacting with the Cloud Auth API.
cloudAuthClient <- function() {
  service <- parseHttpUrl(connectCloudUrls()$auth)
  authInfo <- list()

  list(
    createDeviceAuth = function() {
      # Create form-encoded body
      client_id <- getClientId()
      content <- paste0("client_id=", getClientId(), "&scope=vivid")

      response <- POST(
        service,
        list(),
        path = "/oauth/device/authorize",
        contentType = "application/x-www-form-urlencoded",
        content = content
      )

      response
    },

    exchangeToken = function(request) {
      client_id <- getClientId()
      content <- paste0(
        "client_id=",
        client_id,
        "&grant_type=",
        request$grant_type,
        "&scope=vivid"
      )

      if (!is.null(request$device_code)) {
        content <- paste0(
          content,
          "&device_code=",
          urlEncode(request$device_code)
        )
      }
      if (!is.null(request$refresh_token)) {
        content <- paste0(
          content,
          "&refresh_token=",
          request$refresh_token
        )
      }

      POST(
        service,
        list(),
        path = "/oauth/token",
        contentType = "application/x-www-form-urlencoded",
        content = content
      )
    }
  )
}
