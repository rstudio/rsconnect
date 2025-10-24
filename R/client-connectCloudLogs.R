# Creates a client for interacting with the Connect Cloud Logs API.
connectCloudLogsClient <- function() {
  list(
    getLogs = function(logChannel, authToken) {
      # Parse the logs URL to get service components
      logsUrl <- connectCloudUrls()$logs
      service <- parseHttpUrl(paste0(logsUrl, "/v1"))

      # Create auth info with the authorization token
      authInfo <- list(
        accessToken = authToken
      )

      # Make the GET request to fetch logs
      path <- paste0(
        "/logs/",
        logChannel,
        "?traversal_direction=backward&limit=1500"
      )
      response <- GET(service, authInfo, path)

      # Return the logs data
      response
    }
  )
}
