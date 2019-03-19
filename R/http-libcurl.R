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

  # turn off CURLOPT_FOLLOWLOCATION. the curl package makes this the default for new handles, but it
  # causes a hang when attempting to follow some responses from shinyapps.io.
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
    port <- paste(":", port, sep="")

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

    if (identical(method, "POST")) {
      # for POST operations, send all the file's content up at once. this is necessary because some
      # POST endpoints return 3xx status codes, which require a seekfunction in order to replay the
      # payload (the curl package does not currently allow specifying seekfunctions from R)
      curl::handle_setopt(handle,
                          post = TRUE,
                          postfields = readBin(con,
                                               what = "raw",
                                               n = fileLength),
                          postfieldsize_large = fileLength)
    } else if (identical(method, "PUT")) {
      # for PUT operations, which are often used for larger content (bundle uploads), stream the
      # file from disk instead of reading it from memory
      curl::handle_setopt(handle,
                          upload = TRUE,
                          infilesize_large = fileLength)

      curl::handle_setopt(handle,
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
        })
    } else {
      # why was a file specified for this endpoint?
      warning("Content file specified, but not used because the '", method, "' request ",
              "type does not accept a body.")
    }
  }

  # ensure we're using the requested method
  curl::handle_setopt(handle, customrequest = method)

  # apply all our accumulated headers
  curl::handle_setheaders(handle, .list = headers)

  # make the request
  response <- NULL
  time <- system.time(gcFirst = FALSE, tryCatch({
      # fetch the response into a raw buffer in memory
      response <- curl::curl_fetch_memory(url, handle = handle)
    },
    error = function(e, ...) {
      # ignore errors resulting from timeout or user abort
      if (identical(e$message, "Callback aborted") ||
          identical(e$message, "transfer closed with outstanding read data remaining"))
        return(NULL)
      # bubble remaining errors through
      else
        stop(e)
    }))
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
  if (!is.null(file) && httpTraceJson() &&
      identical(contentType, "application/json"))
    cat(paste0("<< ", paste(readLines(contentFile), collapse="\n"), "\n"))

  # Parse cookies from header; bear in mind that there may be multiple headers
  cookieHeaders <- headers[names(headers) == "set-cookie"]
  storeCookies(list(protocol=protocol, host=host, port=port, path=path), cookieHeaders)

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


