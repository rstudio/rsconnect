# Internal sockets HTTP transport. Not recommended since it does not support HTTPS, but useful as a
# last resort if other methods don't work since it can piggyback on IE proxy settings via R's own
# internet configuration.

httpInternal <- function(
  protocol,
  host,
  port,
  method,
  path,
  headers,
  contentType = NULL,
  contentFile = NULL,
  certificate = NULL,
  timeout = NULL
) {
  if (!is.null(contentFile) && is.null(contentType)) {
    stop("You must specify a contentType for the specified file")
  }

  # default port to 80 if necessary
  if (!nzchar(port)) {
    port <- "80"
  }

  # read file in binary mode
  if (!is.null(contentFile)) {
    fileLength <- file.info(contentFile)$size
    fileContents <- readBin(contentFile, what = "raw", n = fileLength)
  }

  # build http request
  request <- NULL
  request <- c(request, paste(method, " ", path, " HTTP/1.1\r\n", sep = ""))
  request <- c(request, "User-Agent: ", userAgent(), "\r\n")
  request <- c(request, "Host: ", host, "\r\n", sep = "")
  request <- c(request, "Accept: */*\r\n")
  if (!is.null(contentFile)) {
    request <- c(
      request,
      paste("Content-Type: ", contentType, "\r\n", sep = "")
    )
    request <- c(
      request,
      paste("Content-Length: ", fileLength, "\r\n", sep = "")
    )
  }
  headers <- appendCookieHeaders(
    list(protocol = protocol, host = host, port = port, path = path),
    headers
  )
  for (name in names(headers)) {
    request <- c(request, paste(name, ": ", headers[[name]], "\r\n", sep = ""))
  }
  request <- c(request, "\r\n")

  # output request if in verbose mode
  if (httpVerbose()) {
    cat(request)
  }

  # use timeout if supplied, default timeout if not (matches parameter behavior
  # for socketConnection)
  timeout <- if (is.null(timeout)) getOption("timeout") else timeout

  # open socket connection
  time <- system.time(gcFirst = FALSE, {
    conn <- socketConnection(
      host = host,
      port = as.integer(port),
      open = "w+b",
      blocking = TRUE,
      timeout = timeout
    )
    defer(close(conn))

    # write the request header and file payload
    writeBin(charToRaw(paste(request, collapse = "")), conn, size = 1)
    if (!is.null(contentFile)) {
      writeBin(fileContents, conn, size = 1)
    }

    # read the response
    response <- readHttpResponse(
      list(
        protocol = protocol,
        host = host,
        port = port,
        method = method,
        path = path
      ),
      conn
    )
  })
  httpTrace(method, path, time)

  # print if in verbose mode
  if (httpVerbose()) {
    print(response)
  }

  # output JSON if requested
  if (httpTraceJson() && identical(contentType, "application/json")) {
    cat(paste0("<< ", rawToChar(fileContents), "\n"))
  }

  # return it
  response
}
