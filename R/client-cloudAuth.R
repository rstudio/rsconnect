# Map rsconnect appMode to Connect Cloud contentType

getClientId <- function() {
  switch(
    connectCloudEnvironment(),
    production = "posit-publisher",
    staging = "posit-publisher-staging",
    development = "posit-publisher-development",
  )
}

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
        urlEncode(client_id),
        "&grant_type=urn:ietf:params:oauth:grant-type:device_code"
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
          urlEncode(request$refresh_token)
        )
      }

      response <- POST(
        service,
        list(),
        path = "/oauth/token",
        contentType = "application/x-www-form-urlencoded",
        content = content
      )

      response
    }
  )
}
