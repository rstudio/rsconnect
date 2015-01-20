
# This functions are intended to be called primarily by the RStudio IDE.

validateServerUrl <- function(url) {
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
    c(list(valid = TRUE), response)
  }
}

findLocalServer <- function(url) {
  # helper to find a server given its URL
  findServerByUrl <- function(name) {
    allServers <- rsconnect::servers(local = TRUE)
    match <- allServers[allServers$url == url, , drop.names = FALSE]
    if (nrow(length(match)) == 0)
      NULL
    else
      as.character(match[1,"name"])
  }

  # if there are no local servers with the given URL, add one and return its
  # name
  name <- findServerByUrl(url)
  if (is.null(name)) {
    rsconnect::addServer(url, NULL, TRUE)
    findServerByUrl(url)
  } else {
    name
  }
}
