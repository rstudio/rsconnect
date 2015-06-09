
# These functions are intended to be called primarily by the RStudio IDE.

# attempts to validate a server URL by hitting a known configuration endpoint
# (which does not require authentication). returns a list containing (valid =
# TRUE) and server settings, or a list containing (valid = FALSE) and an error
# message.
validateServerUrl <- function(url) {
  # if no protocol specified, guess HTTP (TODO: unfortunately guessing the wrong
  # protocol results in a hung request; is there a way we can test for HTTPS
  # support without hanging if it doesn't exist?)
  if (!grepl("://", url, fixed = TRUE))
    url <- paste0("http://", url)

  url <- ensureConnectServerUrl(url)
  response <- NULL
  errMessage <- ""
  tryCatch({
    response <- handleResponse(
      GET(parseHttpUrl(url), NULL, "/server_settings"))
  }, error = function(e) {
    errMessage <<- e$message
  })
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


