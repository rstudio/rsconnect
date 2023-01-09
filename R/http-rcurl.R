# HTTP transport using the RCurl package. DEPRECATED; exists only for backwards compatibility (it
# was the default transport for many years). In a future release of rsconnect, the RCurl transport
# will be removed entirely, and the "rcurl" option will be interpreted as "libcurl".

httpRCurl <- function(protocol,
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
  url <- paste(protocol, "://", host, port, path, sep = "")

  # read file in binary mode
  if (!is.null(contentFile)) {
    fileLength <- file.info(contentFile)$size
    fileContents <- readBin(contentFile, what = "raw", n = fileLength)
    headers$`Content-Type` <- contentType
  }

  # establish options
  options <- RCurl::curlOptions(url)
  options$useragent <- userAgent()

  # overlay user-supplied options
  userOptions <- getOption("rsconnect.rcurl.options")
  if (is.list(userOptions)) {
    for (option in names(userOptions)) {
      options[option] <- userOptions[option]
    }
  }

  if (isTRUE(getOption("rsconnect.check.certificate", TRUE))) {
    options$ssl.verifypeer <- TRUE

    # apply certificate information if present
    if (!is.null(certificate))
      options$cainfo <- certificate
  } else {
    # don't verify peer (less secure but tolerant to self-signed cert issues)
    options$ssl.verifypeer <- FALSE
  }

  headerGatherer <- RCurl::basicHeaderGatherer()
  options$headerfunction <- headerGatherer$update

  # the text processing done by .mapUnicode has the unfortunate side effect
  # of turning escaped backslashes into ordinary backslashes but leaving
  # ordinary backslashes alone, which can create malformed JSON.
  textGatherer <- RCurl::basicTextGatherer(.mapUnicode = FALSE)

  # use timeout if supplied
  if (!is.null(timeout)) {
    options$timeout <- timeout
  }

  # verbose if requested
  if (httpVerbose())
    options$verbose <- TRUE

  # add extra headers
  headers <- appendCookieHeaders(
    list(protocol = protocol, host = host, port = port, path = path), headers)
  extraHeaders <- as.character(headers)
  names(extraHeaders) <- names(headers)
  options$httpheader <- extraHeaders

  # make the request
  time <- system.time(gcFirst = FALSE, tryCatch({
    if (!is.null(contentFile)) {
      RCurl::curlPerform(url = url,
                         .opts = options,
                         customrequest = method,
                         readfunction = fileContents,
                         infilesize = fileLength,
                         writefunction = textGatherer$update,
                         upload = TRUE)
    } else if (method == "DELETE") {
      RCurl::curlPerform(url = url,
                         .opts = options,
                         customrequest = method)

    } else {
      if (identical(method, "GET")) {
        RCurl::getURL(url,
                      .opts = options,
                      write = textGatherer)
      } else {
        RCurl::curlPerform(url = url,
                           .opts = options,
                           customrequest = method,
                           writefunction = textGatherer$update)
      }
    }},
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
  headers <- headerGatherer$value()

  # deduce status. we do this *before* lowercase conversion, as it is possible
  # for both "Status" and "status" headers to exist
  status <- 200
  statuses <- headers[names(headers) == "status"]   # find status header
  statuses <- statuses[grepl("^\\d+$", statuses)]   # ensure fully numeric
  if (length(statuses) > 0) {
    # we found a numeric status header
    status <- as.integer(statuses[[1]])
  }

  # lowercase all header names for normalization; HTTP/2 uses lowercase headers
  # by default but they're typically capitalized in HTTP/1
  names(headers) <- tolower(names(headers))

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
  if (!is.null(contentFile) && httpTraceJson() &&
      identical(contentType, "application/json"))
    cat(paste0("<< ", rawToChar(fileContents), "\n"))

  # Parse cookies from header; bear in mind that there may be multiple headers
  cookieHeaders <- headers[names(headers) == "set-cookie"]
  storeCookies(list(protocol = protocol, host = host, port = port, path = path), cookieHeaders)

  contentValue <- textGatherer$value()

  # emit JSON trace if requested
  if (httpTraceJson() && identical(contentType, "application/json"))
    cat(paste0(">> ", contentValue, "\n"))

  list(req = list(protocol = protocol,
                  host     = host,
                  port     = port,
                  method   = method,
                  path     = path),
       status = status,
       location = location,
       contentType = contentType,
       content = contentValue)
}
