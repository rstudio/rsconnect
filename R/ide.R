# These functions are intended to be called primarily by the RStudio IDE.

# This function is poorly named because as well as validating the server
# url it will also register the server if needed.
validateServerUrl <- function(url, certificate = NULL) {
  valid <- validateConnectUrl(url, certificate)

  if (valid$ok)  {
    name <- findAndRegisterLocalServer(url)
    c(list(valid = TRUE, url = valid$url, name = name), valid$response)
  } else {
    valid
  }
}

# Validate a connect server URL by hitting a known configuration endpoint
# The URL may be specified with or without the protocol and port; this function
# will try both http and https and follow any redirects given by the server.
validateConnectUrl <- function(url, certificate = NULL) {
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

  # populate certificate if supplied
  certificate <- inferCertificateContents(certificate)

  # begin trying URLs to discover the correct one
  response <- NULL
  errMessage <- ""
  retry <- TRUE
  while (retry) {
    tryCatch({
      response <- GET(parseHttpUrl(url),
                          list(certificate = certificate),
                          settingsEndpoint,
                          timeout = getOption("rsconnect.http.timeout", 10))

      httpResponse <- attr(response, "httpResponse")
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
      if (!isContentType(httpResponse, "application/json")) {
        response <- NULL
        errMessage <- "Endpoint did not return JSON"
      }

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
    list(valid = FALSE, message = errMessage)
  } else {
    list(valid = TRUE, url = url, response = response)
  }
}

# given a server URL, returns that server's short name. if the server is not
# currently registered, the server is registered and the short name of the newly
# registered server is returned.
findAndRegisterLocalServer <- function(url) {
  # helper to find a server given its URL
  findServerByUrl <- function(name) {
    allServers <- rsconnect::servers(local = TRUE)
    match <- allServers[allServers$url == url, , drop = FALSE]
    if (nrow(match) == 0)
      NULL
    else
      as.character(match[1, "name"])
  }

  # if there are no local servers with the given URL, add one and return its
  # name
  name <- findServerByUrl(url)
  if (is.null(name)) {
    addServer(url = url, name = NULL, certificate = NULL,
                     quiet = TRUE, validate = FALSE)
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
