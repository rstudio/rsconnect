
# These functions are intended to be called primarily by the RStudio IDE.

# attempts to validate a server URL by hitting a known configuration endpoint
# (which does not require authentication). returns a list containing (valid =
# TRUE) and server settings, or a list containing (valid = FALSE) and an error
# message.
#
# the URL may be specified with or without the protocol and port; this function
# will try both http and https and follow any redirects given by the server.
validateServerUrl <- function(url) {
  tryAllProtocols <- TRUE

  if (!grepl("://", url, fixed = TRUE))
  {
    if (grepl(":3939", url, fixed = TRUE)) {
      # assume http for default (local) connect installations
      url <- paste0("http://", url)
    } else {
      # assume https elsewhere
      url <- paste0("https://", url)
    }
  }

  # if the URL ends with a port number, don't try http/https on the same port
  if (grepl(":\\d+/?$", url)) {
    tryAllProtocols <- FALSE
  }

  settingsEndpoint <- "/server_settings"
  url <- ensureConnectServerUrl(url)

  # begin trying URLs to discover the correct one
  response <- NULL
  errMessage <- ""
  retry <- TRUE
  while (retry) {
    tryCatch({
      # this shouldn't take more than 5 seconds since it does no work (i.e we
      # should just be waiting for the network), so timeout quickly to avoid
      # hanging when the server doesn't accept the connection
      httpResponse <- GET(parseHttpUrl(url), NULL, settingsEndpoint,
                          timeout = 5)

      # check for redirect
      if (httpResponse$status == 307 &&
          !is.null(httpResponse$location)) {

        # we were served a redirect; try again with the new URL
        url <- httpResponse$location
        if (substring(url, (nchar(url) - nchar(settingsEndpoint)) + 1)   ==
            settingsEndpoint) {
          # chop /server_settings from the redirect path to get the raw API path
          url <- substring(url, 1, nchar(url) - nchar(settingsEndpoint))
        }
        next
      }
      response <- handleResponse(httpResponse)

      # got a real response; stop trying now
      retry <- FALSE
    }, error = function(e) {
      if (inherits(e, "OPERATION_TIMEDOUT") && tryAllProtocols) {
        # if the operation timed out on one protocol, try the other one (note
        # that we don't do this if a port is specified)
        if (substring(url, 1, 7) == "http://") {
          url <<- paste0("https://", substring(url, 8))
        } else if (substring(url, 1, 8) == "https://") {
          url <<- paste0("http://", substring(url, 9))
        }
        tryAllProtocols <<- FALSE
        return()
      }
      errMessage <<- e$message
      retry <<- FALSE
    })
  }
  if (is.null(response)) {
    list(
      valid = FALSE,
      message = errMessage)
  } else {
    c(list(valid = TRUE,
           url = url,
           name = findLocalServer(url)),
      response)
  }
}

# given a server URL, returns that server's short name. if the server is not
# currently registered, the server is registered and the short name of the newly
# registered server is returned.
findLocalServer <- function(url) {
  # make sure the url has the current API suffix
  url <- ensureConnectServerUrl(url)

  # helper to find a server given its URL
  findServerByUrl <- function(name) {
    allServers <- as.data.frame(rsconnect::servers(local = TRUE))
    match <- allServers[allServers$url == url, , drop = FALSE]
    if (nrow(match) == 0)
      NULL
    else
      as.character(match[1,"name"])
  }

  # if there are no local servers with the given URL, add one and return its
  # name
  name <- findServerByUrl(url)
  if (is.null(name)) {
    addConnectServer(url, NULL, TRUE)
    findServerByUrl(url)
  } else {
    name
  }
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

  rstudioapi::callFun("sourceMarkers",
                      name = "Publish Content Issues",
                      markers = markers,
                      basePath = basePath,
                      autoSelect = "first")
}


