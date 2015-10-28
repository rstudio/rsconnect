handleResponse <- function(response, jsonFilter = NULL) {

  # function to report errors
  reportError <- function(msg) {
    stop("HTTP ", response$status, "\n",
         response$req$method, " ",  response$req$protocol, "://",
         response$req$host, response$req$port, response$req$path, "\n",
         msg, call. = FALSE)
  }

  # json responses
  if (isContentType(response, "application/json")) {

    json <- RJSONIO::fromJSON(response$content, simplify = FALSE)

    if (response$status %in% 200:399)
      if (!is.null(jsonFilter))
        jsonFilter(json)
      else
        json
    else if (!is.null(json$error))
      reportError(json$error)
    else
      reportError(paste("Unexpected json response:", response$content))
  }

  # for html responses we can attempt to extract the body
  else if (isContentType(response, "text/html")) {

    body <- regexExtract(".*?<body>(.*?)</body>.*", response$content)
    if (response$status >= 200 && response$status < 400){
      # Good response, return the body if we have one, or the content if not
      if (!is.null(body)){
        body
      } else{
        response$content
      }
    } else {
      # Error response
      if (!is.null(body))
        reportError(body)
      else
        reportError(response$content)
    }
  }

  # otherwise just dump the whole thing
  else {
    if (response$status %in% 200:399)
      response$content
    else
      reportError(response$content)
  }
}
