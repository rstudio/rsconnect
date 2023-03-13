createCurlHandle <- function(timeout, certificate) {
  # create a new curl handle
  handle <- curl::new_handle()

  # set rsconnect user agent
  curl::handle_setopt(handle, useragent = userAgent())

  # overlay user-supplied options
  userOptions <- getOption("rsconnect.libcurl.options")
  if (is.list(userOptions)) {
    curl::handle_setopt(handle, .list = userOptions)
  }

  if (isTRUE(getOption("rsconnect.check.certificate", TRUE))) {
    curl::handle_setopt(handle, ssl_verifypeer = TRUE)

    # apply certificate information if present
    if (!is.null(certificate))
      curl::handle_setopt(handle, cainfo = certificate)
  } else {
    # don't verify peer (less secure but tolerant to self-signed cert issues)
    curl::handle_setopt(handle, ssl_verifypeer = FALSE)
  }

  # use timeout if supplied
  if (!is.null(timeout)) {
    curl::handle_setopt(handle, timeout = timeout)
  }

  # verbose if requested
  if (httpVerbose())
    curl::handle_setopt(handle, verbose = TRUE)

  # suppress curl's automatically handling of redirects, since we have to
  # handle ourselves in httpRequest()/httpRequestWithBody() due to our
  # specialised auth needs
  curl::handle_setopt(handle, followlocation = FALSE)

  # return the newly created handle
  handle
}

httpLibCurl <- function(protocol,
                      host,
                      port,
                      method,
                      path,
                      headers,
                      contentType = NULL,
                      contentFile = NULL,
                      certificate = NULL,
                      timeout = NULL) {

  if (!is.null(contentFile) && is.null(contentType))
    stop("You must specify a contentType for the specified file")

  # add prefix to port if necessary
  if (!is.null(port) && nzchar(port))
    port <- paste(":", port, sep = "")

  # build url
  url <- paste0(protocol, "://", host, port, path)

  # create curl handle
  handle <- createCurlHandle(timeout, certificate)

  # add cookie headers
  headers <- appendCookieHeaders(
    list(protocol = protocol, host = host, port = port, path = path),
    headers)

  if (!is.null(contentFile)) {
    # compute file metadata
    fileLength <- file.info(contentFile)$size
    headers$`Content-Type` <- contentType
    headers$`Content-Length` <- as.character(fileLength)

    # open a connection to read the file, and ensure it's closed when we're done
    con <- file(contentFile, "rb")
    on.exit(if (!is.null(con)) close(con), add = TRUE)

    curl::handle_setopt(
      handle,
      noprogress = fileLength <= 10 * 1024^2,
      upload = TRUE,
      infilesize_large = fileLength,
      readfunction = function(nbytes, ...) {
        if (is.null(con)) {
          return(raw())
        }
        bin <- readBin(con, "raw", nbytes)
        if (length(bin) < nbytes) {
          close(con)
          con <<- NULL
        }
        bin
      }
    )
  }

  # ensure we're using the requested method
  curl::handle_setopt(handle, customrequest = method)

  # apply all our accumulated headers
  curl::handle_setheaders(handle, .list = headers)

  # make the request
  response <- NULL
  time <- system.time(
    response <- curl::curl_fetch_memory(url, handle = handle),
    gcFirst = FALSE
  )
  httpTrace(method, path, time)

  # get list of HTTP response headers
  headers <- curl::parse_headers_list(rawToChar(response$headers))

  # read normalized location header
  if ("location" %in% names(headers))
    location <- headers[["location"]]
  else
    location <- NULL

  # presume a plain text response unless specified otherwise
  contentType <- if ("content-type" %in% names(headers)) {
    headers[["content-type"]]
  } else {
    "text/plain"
  }

  # emit JSON trace if requested
  jsonTracingEnabled <-
    httpTraceJson() &&
    !is.null(contentFile) &&
    identical(contentType, "application/json")

  if (jsonTracingEnabled)
    cat(paste0("<< ", paste(readLines(contentFile), collapse = "\n"), "\n"))

  # Parse cookies from header; bear in mind that there may be multiple headers
  cookieHeaders <- headers[names(headers) == "set-cookie"]
  storeCookies(list(protocol = protocol, host = host, port = port, path = path), cookieHeaders)

  contentValue <- rawToChar(response$content)

  # emit JSON trace if requested
  if (httpTraceJson() && identical(contentType, "application/json"))
    cat(paste0(">> ", contentValue, "\n"))

  list(req = list(protocol = protocol,
                  host     = host,
                  port     = port,
                  method   = method,
                  path     = path),
       status = response$status_code,
       location = location,
       contentType = contentType,
       content = contentValue)
}
