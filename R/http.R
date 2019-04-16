userAgent <- function() {
  paste("rsconnect", packageVersion("rsconnect"), sep="/")
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

httpFunction <- function() {
  httpType <- getOption("rsconnect.http", "libcurl")
  if (identical("libcurl", httpType))
    httpLibCurl
  else if (identical("rcurl", httpType))
    httpRCurl
  else if (identical("curl",  httpType))
    httpCurl
  else if (identical("internal", httpType))
    httpInternal
  else if (is.function(httpType))
    httpType
  else
    stop(paste("Invalid http option specified:",httpType,
               ". Valid values are libcurl, rcurl, curl, and internal"))
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

  # check if the request needs a body
  if ((is.null(file) && is.null(content))) {
      # no file or content, don't include a body with the request
      httpRequest(service, authInfo, "POST", path, query, headers)
  } else {
      # include the request's data in the body
      httpRequestWithBody(service, authInfo, "POST", path, query, contentType, file, content, headers)
  }
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
                timeout = NULL) {
  httpRequest(service, authInfo, "GET", path, query, headers, timeout)
}

DELETE <- function(service,
                   authInfo,
                   path,
                   query = NULL,
                   headers = list()) {
  httpRequest(service, authInfo, "DELETE", path, query, headers)
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

httpRequest <- function(...) {
  httpInvokeRequest(..., http = httpFunction())
}

httpInvokeRequest <- function(service,
                              authInfo,
                              method,
                              path,
                              query,
                              headers = list(),
                              timeout = NULL, 
                              http) {

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

  # perform method
  http(service$protocol,
       service$host,
       service$port,
       method,
       url,
       headers,
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
  curl::curl_unescape(x)
}

urlEncode <- function(x) {
  if (inherits(x, "AsIs")) return(x)
  RCurl::curlEscape(x)
}

queryString <- function (elements) {
  stopifnot(is.list(elements))
  elements <- elements[!sapply(elements, is.null)]

  names <- curl::curl_escape(names(elements))
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

