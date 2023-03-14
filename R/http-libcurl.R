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
  request <- list(
    protocol = protocol,
    host = host,
    port = port,
    method = method,
    path = path
  )

  # create curl handle
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
    if (!is.null(certificate)) {
      curl::handle_setopt(handle, cainfo = certificate)
    }
  } else {
    # don't verify peer (less secure but tolerant to self-signed cert issues)
    curl::handle_setopt(handle, ssl_verifypeer = FALSE)
  }

  # use timeout if supplied
  if (!is.null(timeout)) {
    curl::handle_setopt(handle, timeout = timeout)
  }

  # verbose if requested
  if (httpVerbose()) {
    curl::handle_setopt(handle, verbose = TRUE)
  }

  # suppress curl's automatically handling of redirects, since we have to
  # handle ourselves in httpRequest()/httpRequestWithBody() due to our
  # specialised auth needs
  curl::handle_setopt(handle, followlocation = FALSE)

  # add cookie headers
  headers <- appendCookieHeaders(request, headers)

  if (!is.null(contentFile)) {
    if (is.null(contentType)) {
      stop("You must specify a contentType for the specified file")
    }

    # compute file metadata
    fileLength <- file.info(contentFile)$size
    headers$`Content-Type` <- contentType
    headers$`Content-Length` <- as.character(fileLength)

    # open a connection to read the file, and ensure it's closed when we're done
    con <- file(contentFile, "rb")
    on.exit(if (!is.null(con)) close(con), add = TRUE)

    progress <- is_interactive() && fileLength >= 10 * 1024^2

    curl::handle_setopt(
      handle,
      noprogress = !progress,
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
  url <- buildHttpUrl(request)

  time <- system.time(
    response <- curl::curl_fetch_memory(url, handle = handle),
    gcFirst = FALSE
  )
  httpTrace(method, path, time)

  # Process headers
  headers <- curl::parse_headers_list(rawToChar(response$headers))

  # Parse cookies from header; bear in mind that there may be multiple headers
  cookieHeaders <- headers[names(headers) == "set-cookie"]
  storeCookies(request, cookieHeaders)

  # presume a plain text response unless specified otherwise
  contentType <- headers[["content-type"]] %||% "text/plain"
  contentValue <- rawToChar(response$content)

  # emit JSON trace if requested
  jsonTracingEnabled <- httpTraceJson() && contentType == "application/json"
  if (jsonTracingEnabled) {
    if (!is.null(contentFile)) {
      cat(paste0("<< ", readLines(contentFile, warn = FALSE), "\n", collapse = ""))
    }
    lines <- strsplit(contentValue, "\n")[[1]]
    cat(paste0(">> ", lines, "\n", collapse = ""))
  }

  list(
    req = request,
    status = response$status_code,
    location = headers$location,
    contentType = contentType,
    content = contentValue
  )
}
