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
  # Add protocol if missing, assuming https except for local installs
  if (!grepl("://", url, fixed = TRUE)) {
    if (grepl(":3939", url, fixed = TRUE)) {
      url <- paste0("http://", url)
    } else {
      url <- paste0("https://", url)
    }
  }
  url <- ensureConnectServerUrl(url)
  is_http <- grepl("^http://", url)

  GET_server_settings <- function(url) {
    auth_info <- list(certificate = inferCertificateContents(certificate))
    GET(
      parseHttpUrl(url),
      auth_info,
      "/server_settings",
      timeout = getOption("rsconnect.http.timeout", 10)
    )
  }

  response <- NULL
  cnd <- catch_cnd(response <- GET_server_settings(url), "error")
  if (is_http && cnd_inherits(cnd, "OPERATION_TIMEDOUT")) {
    url <- gsub("^http://", "https://", url)
    cnd <- catch_cnd(response <- GET_server_settings(url), "error")
  }

  if (!is.null(cnd)) {
    return(list(valid = FALSE, message = conditionMessage(cnd)))
  }

  httpResponse <- attr(response, "httpResponse")
  if (!isContentType(httpResponse, "application/json")) {
    return(list(valid = FALSE, message = "Endpoint did not return JSON"))
  }

  url <- gsub("/server_settings$", "", buildHttpUrl(httpResponse$req))
  list(valid = TRUE, url = url, response = response)
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
