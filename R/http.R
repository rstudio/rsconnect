# Environment in which cookies will be stored. Cookies are expected to survive
# the duration of the R session, but are not persisted outside of the R
# session.
.cookieStore <- new.env(parent=emptyenv())

# Returns the cookies associated with a particular host/port
# If no hostname is specified, returns all cookies
getCookies <- function(hostname, port=NULL){
  if (missing(hostname)){
    hosts <- ls(envir=.cookieStore)
    cookies <- lapply(hosts, function(h){
      getCookiesHostname(h)
    })
    do.call("rbind", cookies)
  } else {
    host <- getCookieHost(list(host=hostname, port=port))
    getCookiesHostname(host)
  }
}

# Get cookies for a particular hostname(:port)
getCookiesHostname <- function(host){
  if (!exists(host, .cookieStore)){
    NULL
  } else {
    cookies <- get(host, envir=.cookieStore)
    cookies$host <- host
    cookies
  }
}

# Clears the cookies associated with a particular hostname/port combination.
# If hostname and port are omitted, clears all the cookies
clearCookies <- function(hostname, port=NULL){
  if (missing(hostname)){
    rm(list=ls(envir=.cookieStore), envir=.cookieStore)
  } else {
    host <- getCookieHost(list(host=hostname, port=port))
    rm(list=host, envir=.cookieStore)
  }
}

userAgent <- function() {
  paste("rsconnect", packageVersion("rsconnect"), sep="/")
}

getCookieHost <- function(requestURL){
  host <- requestURL$host
  port <- requestURL$port
  if (!is.null(port) && nchar(port) > 0){
    port <- sub("^:", "", port)
    # By my reading of the RFC, we technically only need to include the port #
    # in the index if the host is an IP address. But here we're including the
    # port number as a part of the host whether using a domain name or IP.
    # Erring on the side of not sending the cookies to the wrong services
    host <- paste(host, port, sep=":")
  }
  host
}

# Parse out the raw headers provided and insert them into the cookieStore
# NOTE: Domain attribute is currently ignored
# @param requestURL the parsed URL as returned from `parseHttpUrl`
# @param cookieHeaders a list of characters strings representing the raw
#   Set-Cookie header value with the "Set-Cookie: " prefix omitted
storeCookies <- function(requestURL, cookieHeaders){
  cookies <- lapply(cookieHeaders, function(co){ parseCookie(requestURL, co) })

  # Filter out invalid cookies (which would return as NULL)
  cookies <- Filter(Negate(is.null), cookies)

  host <- getCookieHost(requestURL)

  hostCookies <- NULL
  if (!exists(host, .cookieStore)){
    # Create a new data frame for this host
    hostCookies <- data.frame(
      path=character(0),
      name=character(0),
      value=character(0),
      secure=logical(0),
      expires=character(0),
      stringsAsFactors = FALSE
    )
  } else {
    hostCookies <- get(host, envir=.cookieStore)
  }

  lapply(cookies, function(co){
    # Remove any duplicates
    # RFC says duplicate cookies are ones that have the same domain, name, and path
    hostCookies <<- hostCookies[!(co$name == hostCookies$name & co$path == hostCookies$path),]

    # append this new cookie on
    hostCookies <<- rbind(as.data.frame(co, stringsAsFactors=FALSE), hostCookies)
  })

  # Save this host's cookies into the cookies store.
  assign(host, hostCookies, envir=.cookieStore)
}

# Parse out an individual cookie
# @param requestURL the parsed URL as returned from `parseHttpUrl`
# @param cookieHeader the raw text contents of the Set-Cookie header with the
#   header name omitted.
parseCookie <- function(requestURL, cookieHeader){
  keyval <- regmatches(cookieHeader, regexec(
    # https://curl.haxx.se/rfc/cookie_spec.html
    # "characters excluding semi-colon, comma and white space"
    # white space is not excluded from values so we can capture `expires`
    "^([^;=, ]+)\\s*=\\s*([^;,]*)(;|$)", cookieHeader, ignore.case=TRUE))[[1]]
  if (length(keyval) == 0){
    # Invalid cookie format.
    warning("Unable to parse set-cookie header: ", cookieHeader)
    return(NULL)
  }
  key <- keyval[2]
  val <- keyval[3]

  # Path
  path <- regmatches(cookieHeader, regexec(
    "^.*\\sPath\\s*=\\s*([^;]+)(;|$).*$", cookieHeader, ignore.case=TRUE))[[1]]
  if (length(path) == 0){
    path <- "/"
  } else {
    path <- path[2]
  }
  if (!substring(requestURL$path, 1, nchar(path)) == path){
    # Per the RFC, the cookie's path must be a prefix of the request URL
    warning("Invalid path set for cookie on request for '", requestURL$path, "': ", cookieHeader)
    return(NULL)
  }

  # MaxAge
  maxage <- regmatches(cookieHeader, regexec(
    "^.*\\sMax-Age\\s*=\\s*(-?\\d+)(;|$).*$", cookieHeader, ignore.case=TRUE))[[1]]
  # If no maxage specified, then this is a session cookie, which means that
  # (since our cookies only survive for a single session anyways...) we should
  # keep this cookie around as long as we're alive.
  expires <- Sys.time() + 10^10
  if (length(maxage) > 0){
    # Compute time maxage seconds from now
    expires <- Sys.time() + as.numeric(maxage[2])
  }

  # Secure
  secure <- grepl(";\\s+Secure(;|$)", cookieHeader, ignore.case=TRUE)

  list(name=key,
       value=val,
       expires=expires,
       path=path,
       secure=secure)
}

# Appends a cookie header from the .cookieStore to the existing set of headers
# @param requestURL the parsed URL as returned from `parseHttpUrl`
# @param headers a named character vector containing the set of headers to be extended
appendCookieHeaders <- function(requestURL, headers){
  host <- getCookieHost(requestURL)

  if (!exists(host, .cookieStore)){
    # Nothing to do
    return(headers)
  }

  cookies <- get(host, envir=.cookieStore)

  # If any cookies are expired, remove them from the cookie store
  if (any(cookies$expires < as.integer(Sys.time()))){
    cookies <- cookies[cookies$expires >= as.integer(Sys.time()),]
    # Update the store, removing the expired cookies
    assign(host, cookies, envir=.cookieStore)
  }

  if (nrow(cookies) == 0){
    # Short-circuit, return unmodified headers.
    return(headers)
  }

  # Filter to only include cookies that match the path prefix
  cookies <- cookies[substring(requestURL$path, 1, nchar(cookies$path)) == cookies$path,]

  # If insecure channel, filter out secure cookies
  if(tolower(requestURL$protocol) != "https"){
    cookies <- cookies[!cookies$secure,]
  }

  # TODO: Technically per the RFC we're supposed to order these cookies by which
  # paths most specifically match the request.
  cookieHeader <- paste(apply(cookies, 1,
                              function(x){ paste0(x["name"], "=", x["value"]) }), collapse="; ")

  if (nrow(cookies) > 0){
    return(c(headers, cookie=cookieHeader))
  } else {
    # Return unmodified headers
    return(headers)
  }
}

parseHttpUrl <- function(urlText) {

  matches <- regexec("(http|https)://([^:/#?]+)(?::(\\d+))?(.*)", urlText)
  components <- regmatches(urlText, matches)[[1]]
  if (length(components) == 0)
    stop("Invalid url: ", urlText)

  url <- list()
  url$protocol <- components[[2]]
  url$host <- components[[3]]
  url$port <- components[[4]]
  url$path <- components[[5]]
  url
}

parseHttpHeader <- function(header) {
  split <- strsplit(header, ": ")[[1]]
  if (length(split) == 2)
    return (list(name = split[1], value = split[2]))
  else
    return (NULL)
}

parseHttpStatusCode <- function(statusLine) {
  # extract status code; needs to deal with HTTP/1.0, HTTP/1.1, and HTTP/2
  statusCode <- regexExtract("HTTP/[0-9]+\\.?[0-9]* ([0-9]+).*", statusLine)
  if (is.null(statusCode))
    return (-1)
  else
    return (as.integer(statusCode))
}

# @param request A list containing protocol, host, port, method, and path fields
# @param conn The connection to read the response from.
readHttpResponse <- function(request, conn) {
  # read status code
  resp <- readLines(conn, 1)
  statusCode <- parseHttpStatusCode(resp[1])

  # read response headers
  contentLength <- NULL
  contentType <- NULL
  location <- NULL
  setCookies <- NULL
  repeat {
    resp <- readLines(conn, 1)
    if (nzchar(resp) == 0)
      break()

    header <- parseHttpHeader(resp)
    if (!is.null(header))
    {
      name <- tolower(header$name)
      if (name == "content-type")
        contentType <- header$value
      if (name == "content-length")
        contentLength <- as.integer(header$value)
      if (name == "location")
        location <- header$value
      if (name == "set-cookie")
        setCookies <- c(setCookies, header$value)
    }
  }

  # Store the cookies that were found in the request
  storeCookies(request, setCookies)

  # read the response content
  if (is.null(contentLength)) {
    # content length is unknown, so stream remaining text
    content <- paste(readLines(con = conn), collapse = "\n")
  } else {
    # we know the content length, so read exactly that many bytes
    content <- rawToChar(readBin(con = conn, what = "raw",
                                 n = contentLength))
  }

  # emit JSON trace if requested
  if (httpTraceJson() && identical(contentType, "application/json"))
    cat(paste0(">> ", content, "\n"))

  # return list
  list(req         = request,
       status      = statusCode,
       location    = location,
       contentType = contentType,
       content     = content)
}


# internal sockets implementation of upload
httpInternal <- function(protocol,
                         host,
                         port,
                         method,
                         path,
                         headers,
                         contentType = NULL,
                         file = NULL,
                         certificate = NULL,
                         writer = NULL,
                         timeout = NULL) {

  if (!is.null(file) && is.null(contentType))
    stop("You must specify a contentType for the specified file")

  # default port to 80 if necessary
  if (!nzchar(port))
    port <- "80"

  # read file in binary mode
  if (!is.null(file)) {
    fileLength <- file.info(file)$size
    fileContents <- readBin(file, what="raw", n=fileLength)
  }

  # build http request
  request <- NULL
  request <- c(request, paste(method, " ", path, " HTTP/1.1\r\n", sep=""))
  request <- c(request, "User-Agent: ", userAgent(), "\r\n")
  request <- c(request, "Host: ", host, "\r\n", sep="")
  request <- c(request, "Accept: */*\r\n")
  if (!is.null(file)) {
    request <- c(request, paste("Content-Type: ",
                                contentType,
                                "\r\n",
                                sep=""))
    request <- c(request, paste("Content-Length: ",
                                fileLength,
                                "\r\n",
                                sep=""))
  }
  headers <- appendCookieHeaders(
    list(protocol=protocol, host=host, port=port, path=path), headers)
  for (name in names(headers))
  {
    request <- c(request,
                 paste(name, ": ", headers[[name]], "\r\n", sep=""))
  }
  request <- c(request, "\r\n")

  # output request if in verbose mode
  if (httpVerbose())
    cat(request)

  # use timeout if supplied, default timeout if not (matches parameter behavior
  # for socketConnection)
  timeout <- if (is.null(timeout)) getOption("timeout") else timeout

  # open socket connection
  time <- system.time(gcFirst=FALSE, {
    conn <- socketConnection(host = host,
                             port = as.integer(port),
                             open = "w+b",
                             blocking = TRUE,
                             timeout = timeout)
    on.exit(close(conn))

    # write the request header and file payload
    writeBin(charToRaw(paste(request,collapse="")), conn, size=1)
    if (!is.null(file)) {
      writeBin(fileContents, conn, size=1)
    }

    # read the response
    response <- readHttpResponse(list(
        protocol = protocol,
        host     = host,
        port     = port,
        method   = method,
        path     = path),
      conn)
  })
  httpTrace(method, path, time)

  # print if in verbose mode
  if (httpVerbose())
    print(response)

  # output JSON if requested
  if (httpTraceJson() && identical(contentType, "application/json"))
    cat(paste0("<< ", rawToChar(fileContents), "\n"))

  # return it
  response
}

httpCurl <- function(protocol,
                     host,
                     port,
                     method,
                     path,
                     headers,
                     contentType = NULL,
                     file = NULL,
                     certificate = NULL,
                     writer = NULL,
                     timeout = NULL) {

  if (!is.null(file) && is.null(contentType))
    stop("You must specify a contentType for the specified file")

  if (!is.null(file))
    fileLength <- file.info(file)$size

  headers <- appendCookieHeaders(
    list(protocol=protocol, host=host, port=port, path=path), headers)
  extraHeaders <- character()
  for (header in names(headers))
  {
    if(!identical(header, "Content-Type") && !identical(header, "Content-Length")){
      extraHeaders <- paste(extraHeaders, "--header")
      extraHeaders <- paste(extraHeaders,
                            paste('"', header,": ",headers[[header]], '"', sep=""))
    }
  }

  outputFile <- tempfile()

  command <- paste("curl",
                   "-i",
                   "-X",
                   method);

  if (httpVerbose())
    command <- paste(command, "-v")

  if (!is.null(timeout))
    command <- paste(command, "--connect-timeout", timeout)

  if (!is.null(file)) {
    command <- paste(command,
                     "--data-binary",
                     shQuote(paste("@", file, sep="")),
                     "--header", paste('"' ,"Content-Type: ",contentType, '"', sep=""),
                     "--header", paste('"', "Content-Length: ", fileLength, '"', sep=""))
  }

  # add prefix to port if necessary
  if (nzchar(port))
    port <- paste(":", port, sep="")

  if (!isTRUE(getOption("rsconnect.check.certificate", TRUE))) {
    # suppressed certificate check
    command <- paste(command, "--insecure")
  } else if (!is.null(certificate)) {
    # cert check not suppressed and we have a supplied cert
    command <- paste(command,
                     "--cacert", shQuote(certificate))
  }

  command <- paste(command,
                   extraHeaders,
                   "--header", "Expect:",
                   "--user-agent", userAgent(),
                   "--silent",
                   "--show-error",
                   "-o", shQuote(outputFile),
                   paste('"', protocol, "://", host, port, path, '"', sep=""))

  result <- NULL
  time <- system.time(gcFirst = FALSE, {
    result <- system(command)
  })
  httpTrace(method, path, time)

  # emit JSON trace if requested
  if (!is.null(file) && httpTraceJson() &&
      identical(contentType, "application/json"))
  {
    fileLength <- file.info(file)$size
    fileContents <- readBin(file, what="raw", n=fileLength)
    cat(paste0("<< ", rawToChar(fileContents), "\n"))
  }

  if (result == 0) {
    fileConn <- file(outputFile, "rb")
    on.exit(close(fileConn))
    readHttpResponse(list(
        protocol = protocol,
        host     = host,
        port     = port,
        method   = method,
        path     = path),
      fileConn)
  } else {
    stop(paste("Curl request failed (curl error", result, "occurred)"))
  }
}

httpRCurl <- function(protocol,
                      host,
                      port,
                      method,
                      path,
                      headers,
                      contentType = NULL,
                      file = NULL,
                      certificate = NULL,
                      writer = NULL,
                      timeout = NULL) {

  if (!is.null(file) && is.null(contentType))
    stop("You must specify a contentType for the specified file")

  # add prefix to port if necessary
  if (!is.null(port) && nzchar(port))
    port <- paste(":", port, sep="")

  # build url
  url <- paste(protocol, "://", host, port, path, sep="")

  # read file in binary mode
  if (!is.null(file)) {
    fileLength <- file.info(file)$size
    fileContents <- readBin(file, what="raw", n=fileLength)
    headers$`Content-Type` <- contentType
  }

  # establish options
  options <- RCurl::curlOptions(url)
  options$useragent <- userAgent()
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
  textGatherer <- if (is.null(writer))
      RCurl::basicTextGatherer(.mapUnicode = FALSE)
    else
      writer

  # when using a custom output writer, add a progress check so we can
  # propagate interrupts, and wait a long time (for streaming)
  if (!is.null(writer)) {
    options$noprogress <- FALSE
    options$progressfunction <- writer$progress
    options$timeout <- 9999999
  }

  # use timeout if supplied
  if (!is.null(timeout)) {
    options$timeout <- timeout
  }

  # verbose if requested
  if (httpVerbose())
    options$verbose <- TRUE

  # add extra headers
  headers <- appendCookieHeaders(
    list(protocol=protocol, host=host, port=port, path=path), headers)
  extraHeaders <- as.character(headers)
  names(extraHeaders) <- names(headers)
  options$httpheader <- extraHeaders

  # make the request
  time <- system.time(gcFirst = FALSE, tryCatch({
    if (!is.null(file)) {
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

  # emit JSON trace if requested
  if (!is.null(file) && httpTraceJson() &&
      identical(contentType, "application/json"))
    cat(paste0("<< ", rawToChar(fileContents), "\n"))

  # return list
  headers <- headerGatherer$value()
  if ("Location" %in% names(headers))
    location <- headers[["Location"]]
  else
    location <- NULL
  # presume a plain text response unless specified otherwise
  contentType <- if ("Content-Type" %in% names(headers)) {
    headers[["Content-Type"]]
  } else {
    "text/plain"
  }

  # Parse cookies from header; bear in mind that there may be multiple headers
  cookieHeaders <- headers[names(headers) == "Set-Cookie"]
  storeCookies(list(protocol=protocol, host=host, port=port, path=path), cookieHeaders)

  contentValue <- textGatherer$value()

  # emit JSON trace if requested
  if (httpTraceJson() && identical(contentType, "application/json"))
    cat(paste0(">> ", contentValue, "\n"))

  list(req = list(protocol = protocol,
                  host     = host,
                  port     = port,
                  method   = method,
                  path     = path),
       status = as.integer(headers[["status"]]),
       location = location,
       contentType = contentType,
       content = contentValue)
}

httpVerbose <- function() {
  getOption("rsconnect.http.verbose", FALSE)
}

httpTraceJson <- function() {
  getOption("rsconnect.http.trace.json", FALSE)
}

httpTrace <- function(method, path, time) {
  if (getOption("rsconnect.http.trace", FALSE)) {
    cat(method, " ", path, " ", as.integer(time[['elapsed']]*1000), "ms\n",
        sep="")
  }
}

defaultHttpFunction <- function() {

  # on Windows, prefer 'curl' if it's available on the PATH
  # as 'RCurl' bundles a version of OpenSSL that's too old.
  # note that newer versions of Windows 10 supply a 'curl' binary
  # by default
  if (identical(Sys.info()[["sysname"]], "Windows")) {
    curl <- Sys.which("curl")
    if (nzchar(curl))
      return("curl")
  }

  # otherwise, default to RCurl for now (pending update to use httr)
  "rcurl"
}

httpFunction <- function() {
  httpType <- getOption("rsconnect.http", defaultHttpFunction())
  if (identical("rcurl", httpType))
    httpRCurl
  else if (identical("curl",  httpType))
    httpCurl
  else if (identical("internal", httpType))
    httpInternal
  else if (is.function(httpType))
    httpType
  else
    stop(paste("Invalid http option specified:",httpType,
               ". Valid values are rcurl, curl, and internal"))
}

POST_JSON <- function(service,
                      authInfo,
                      path,
                      json,
                      query = NULL,
                      headers = list()) {
  POST(service,
       authInfo,
       path,
       query,
       "application/json",
       content = toJSON(json, pretty = TRUE, digits = 30),
       headers = headers)
}

PUT_JSON <- function(service,
                     authInfo,
                     path,
                     json,
                     query = NULL,
                     headers = list()) {
  PUT(service,
      authInfo,
      path,
      query,
      "application/json",
      content = toJSON(json, pretty = TRUE, digits = 30),
      headers = headers)
}

POST <- function(service,
                 authInfo,
                 path,
                 query = NULL,
                 contentType = NULL,
                 file = NULL,
                 content = NULL,
                 headers = list()) {
  httpRequestWithBody(service, authInfo, "POST", path, query, contentType, file, content, headers)
}

PUT <- function(service,
                authInfo,
                path,
                query = NULL,
                contentType = NULL,
                file = NULL,
                content = NULL,
                headers = list()) {
  httpRequestWithBody(service, authInfo, "PUT", path, query, contentType, file, content, headers)
}

GET <- function(service,
                authInfo,
                path,
                query = NULL,
                headers = list(),
                writer = NULL,
                timeout = NULL) {
  httpRequest(service, authInfo, "GET", path, query, headers, writer, timeout)
}

DELETE <- function(service,
                   authInfo,
                   path,
                   query = NULL,
                   headers = list(),
                   writer = NULL) {
  httpRequest(service, authInfo, "DELETE", path, query, headers, writer)
}

httpRequestWithBody <- function(service,
                         authInfo,
                         method,
                         path,
                         query = NULL,
                         contentType = NULL,
                         file = NULL,
                         content = NULL,
                         headers = list()) {

  if ((is.null(file) && is.null(content)))
    stop("You must specify either the file or content parameter.")
  if ((!is.null(file) && !is.null(content)))
    stop("You must specify either the file or content parameter but not both.")

  # prepend the service path
  url <- paste(service$path, path, sep="")

  # append the query
  if (!is.null(query)) {
    # URL encode query args
    query <- utils::URLencode(query)
    url <- paste(url, "?", query, sep="")
  }

  # if we have content then write it to a temp file before posting
  if (!is.null(content)) {
    file <- tempfile()
    writeChar(content, file,  eos = NULL, useBytes=TRUE)
  }

  # if this request is to be authenticated, sign it
  if (!is.null(authInfo$secret) || !is.null(authInfo$private_key)) {
    sigHeaders <- signatureHeaders(authInfo, method, url, file)
    headers <- append(headers, sigHeaders)
  } else {
    headers <- append(headers, bogusSignatureHeaders())
  }

  # set up certificate tempfile if using https
  certificate <- NULL
  if (identical(service$protocol, "https"))
    certificate <- createCertificateFile(authInfo$certificate)

  # perform request
  http <- httpFunction()
  http(service$protocol,
       service$host,
       service$port,
       method,
       url,
       headers,
       contentType,
       file,
       certificate = certificate)
}

httpRequest <- function(service,
                        authInfo,
                        method,
                        path,
                        query,
                        headers = list(),
                        writer = NULL,
                        timeout = NULL) {

  # prepend the service path
  url <- paste(service$path, path, sep="")

  # append the query
  if (!is.null(query)) {
    # URL encode query args
    query <- utils::URLencode(query)
    url <- paste(url, "?", query, sep="")
  }

  # the request should be authenticated if there's any auth specified
  # other than the server certificate
  if (length(authInfo) > 0 &&
      !identical(names(authInfo), "certificate")) {
    sigHeaders <- signatureHeaders(authInfo, method, url, NULL)
    headers <- append(headers, sigHeaders)
  } else {
    headers <- append(headers, bogusSignatureHeaders())
  }

  # set up certificate tempfile if using https
  certificate <- NULL
  if (identical(service$protocol, "https"))
    certificate <- createCertificateFile(authInfo$certificate)

  # perform GET
  http <- httpFunction()
  http(service$protocol,
       service$host,
       service$port,
       method,
       url,
       headers,
       writer = writer,
       timeout = timeout,
       certificate = certificate)
}

rfc2616Date <- function(time = Sys.time()) {

  # capure current locale
  loc <- Sys.getlocale('LC_TIME')

  # set locale to POSIX/C to ensure ASCII date
  Sys.setlocale("LC_TIME", "C")

  # generate date
  date <- strftime(Sys.time(), "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")

  # restore locale
  Sys.setlocale("LC_TIME", loc)

  return(date)
}

urlDecode <- function(x) {
  RCurl::curlUnescape(x)
}

urlEncode <- function(x) {
  if (inherits(x, "AsIs")) return(x)
  RCurl::curlEscape(x)
}

queryString <- function (elements) {
  stopifnot(is.list(elements))
  elements <- elements[!sapply(elements, is.null)]

  names <- RCurl::curlEscape(names(elements))
  values <- vapply(elements, urlEncode, character(1))
  if (length(elements) > 0) {
    result <- paste0(names, "=", values, collapse = "&")
  } else {
    result <- ""
  }
  return(result)
}

bogusSignatureHeaders <- function() {
  list(`X-Auth-Token` = 'anonymous-access') # The value doesn't actually matter here, but the header needs to be set.
}

signatureHeaders <- function(authInfo, method, path, file) {
  # headers to return
  headers <- list()

  # remove query string from path if necessary
  path <- strsplit(path, "?", fixed = TRUE)[[1]][[1]]

  # generate date
  date <- rfc2616Date()

  if (!is.null(authInfo$secret)) {
    # generate contents hash
    if (!is.null(file))
      md5 <- md5sum(file)
    else
      md5 <- openssl::md5("")

    # build canonical request
    canonicalRequest <- paste(method, path, date, md5, sep="\n")

    # sign request using shared secret
    decodedSecret <- openssl::base64_decode(authInfo$secret)
    hmac <- openssl::sha256(canonicalRequest, key = decodedSecret)
    signature <- paste(openssl::base64_encode(hmac), "; version=1", sep="")
  } else if (!is.null(authInfo$private_key)) {
    # generate contents hash (this is done slightly differently for private key
    # auth since we use base64 throughout)
    if (!is.null(file)) {
      con <- base::file(file, open = "rb")
      on.exit(close(con), add = TRUE)
      md5 <- openssl::md5(con)
    }
    else
      md5 <- openssl::md5(raw(0))
    md5 <- openssl::base64_encode(md5)

    # build canonical request
    canonicalRequest <- paste(method, path, date, md5, sep="\n")

    # sign request using local private key
    private_key <- openssl::read_key(
        openssl::base64_decode(authInfo$private_key), der = TRUE)

    # OpenSSL defaults to sha1 hash function (which is what we need)
    rawsig <- openssl::signature_create(charToRaw(canonicalRequest), key = private_key)
    signature <- openssl::base64_encode(rawsig)
  } else {
    stop("can't sign request: no shared secret or private key")
  }

  # return headers
  headers$Date <- date
  headers$`X-Auth-Token` <- authInfo$token
  headers$`X-Auth-Signature` <- signature
  headers$`X-Content-Checksum` <- md5
  headers
}

