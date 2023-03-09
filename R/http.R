
#' @param authInfo Typically an object created by `accountInfo()` augmented
#'   with the `certificate` from the corresponding `serverInfo()`.
#'
#'   There are three different fields used for auth:
#'   * `secret`: set in `setAccountInfo()`
#'   * `private_key`: set in `connectUser()`
#'   * `apiKey`: set in `connectApiUser()`
#'
#' @noRd
httpRequest <- function(service,
                        authInfo,
                        method,
                        path,
                        query,
                        headers = list(),
                        timeout = NULL) {

  path <- buildPath(service$path, path, query)
  headers <- c(headers, authHeaders(authInfo, method, path))
  certificate <- requestCertificate(service$protocol, authInfo$certificate)

  # perform request
  http <- httpFunction()
  http(
    protocol = service$protocol,
    host = service$host,
    port = service$port,
    method = method,
    path = path,
    headers = headers,
    timeout = timeout,
    certificate = certificate
  )
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
  if ((is.null(file) && is.null(content))) {
    stop("You must specify either the file or content parameter.")
  }
  if ((!is.null(file) && !is.null(content))) {
    stop("You must specify either the file or content parameter but not both.")
  }

  # if we have content then write it to a temp file before posting
  if (!is.null(content)) {
    file <- tempfile()
    writeChar(content, file, eos = NULL, useBytes = TRUE)
  }

  path <- buildPath(service$path, path, query)
  headers <- c(headers, authHeaders(authInfo, method, path, file))
  certificate <- requestCertificate(service$protocol, authInfo$certificate)

  # perform request
  http <- httpFunction()
  http(
    protocol = service$protocol,
    host = service$host,
    port = service$port,
    method = method,
    path = path,
    headers = headers,
    contentType = contentType,
    contentFile = file,
    certificate = certificate
  )
}

# Wrappers for HTTP methods -----------------------------------------------

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
    httpRequestWithBody(
      service,
      authInfo,
      "POST",
      path,
      query,
      contentType,
      file,
      content,
      headers
    )
  }
}

POST_JSON <- function(service,
                      authInfo,
                      path,
                      json,
                      query = NULL,
                      headers = list()) {
  POST(
    service,
    authInfo,
    path,
    query,
    "application/json",
    content = toJSON(json, pretty = TRUE, digits = 30),
    headers = headers
  )
}

PUT <- function(service,
                authInfo,
                path,
                query = NULL,
                contentType = NULL,
                file = NULL,
                content = NULL,
                headers = list()) {
  httpRequestWithBody(
    service,
    authInfo,
    "PUT",
    path,
    query,
    contentType,
    file,
    content,
    headers
  )
}

PUT_JSON <- function(service,
                     authInfo,
                     path,
                     json,
                     query = NULL,
                     headers = list()) {
  PUT(
    service,
    authInfo,
    path,
    query,
    "application/json",
    content = toJSON(json, pretty = TRUE, digits = 30),
    headers = headers
  )
}


# User options ------------------------------------------------------------

httpVerbose <- function() {
  getOption("rsconnect.http.verbose", FALSE)
}

httpTraceJson <- function() {
  getOption("rsconnect.http.trace.json", FALSE)
}

httpTrace <- function(method, path, time) {
  if (getOption("rsconnect.http.trace", FALSE)) {
    cat(method, " ", path, " ", as.integer(time[["elapsed"]] * 1000), "ms\n",
      sep = ""
    )
  }
}

httpFunction <- function() {
  httpType <- getOption("rsconnect.http", "libcurl")
  if (identical("libcurl", httpType)) {
    httpLibCurl
  } else if (identical("rcurl", httpType)) {
    httpRCurl
  } else if (identical("curl", httpType)) {
    httpCurl
  } else if (identical("internal", httpType)) {
    httpInternal
  } else if (is.function(httpType)) {
    httpType
  } else {
    stop(paste(
      "Invalid http option specified:", httpType,
      ". Valid values are libcurl, rcurl, curl, and internal"
    ))
  }
}

# URL manipulation --------------------------------------------------------

parseHttpUrl <- function(urlText) {
  matches <- regexec("(http|https)://([^:/#?]+)(?::(\\d+))?(.*)", urlText)
  components <- regmatches(urlText, matches)[[1]]
  if (length(components) == 0) {
    stop("Invalid url: ", urlText)
  }

  url <- list()
  url$protocol <- components[[2]]
  url$host <- components[[3]]
  url$port <- components[[4]]
  url$path <- components[[5]]
  url
}

urlDecode <- function(x) {
  curl::curl_unescape(x)
}

urlEncode <- function(x) {
  if (inherits(x, "AsIs")) {
    return(x)
  }
  RCurl::curlEscape(x)
}

buildPath <- function(apiPath, path, query = NULL) {
  # prepend the service path
  url <- paste(apiPath, path, sep = "")

  # append the query
  if (!is.null(query)) {
    # URL encode query args
    query <- utils::URLencode(query)
    url <- paste(url, "?", query, sep = "")
  }

  url
}

queryString <- function(elements) {
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

# Auth --------------------------------------------------------------------

requestCertificate <- function(protocol, certificate = NULL) {
  if (identical(protocol, "https")) {
    createCertificateFile(certificate)
  } else {
    NULL
  }
}

authHeaders <- function(authInfo, method, path, file = NULL) {
  if (!is.null(authInfo$secret) || !is.null(authInfo$private_key)) {
    signatureHeaders(authInfo, method, path, file)
  } else if (!is.null(authInfo$apiKey)) {
    list(`Authorization` = paste("Key", authInfo$apiKey))
  } else {
    # The value doesn't actually matter here, but the header needs to be set.
    list(`X-Auth-Token` = "anonymous-access")
  }
}

signatureHeaders <- function(authInfo, method, path, file = NULL) {
  # headers to return
  headers <- list()

  # remove query string from path if necessary
  path <- strsplit(path, "?", fixed = TRUE)[[1]][[1]]

  # generate date
  date <- rfc2616Date()

  # generate contents hash as hex values.
  md5 <- fileMD5(file)

  if (!is.null(authInfo$secret)) {
    # the content hash is a string of hex characters when using secret.
    md5 <- md5.as.string(md5)

    # build canonical request
    canonicalRequest <- paste(method, path, date, md5, sep = "\n")

    # sign request using shared secret
    decodedSecret <- openssl::base64_decode(authInfo$secret)
    hmac <- openssl::sha256(canonicalRequest, key = decodedSecret)
    signature <- paste(openssl::base64_encode(hmac), "; version=1", sep = "")
  } else if (!is.null(authInfo$private_key)) {
    # the raw content hash is base64 encoded hex values when using private key.
    md5 <- openssl::base64_encode(md5)

    # build canonical request
    canonicalRequest <- paste(method, path, date, md5, sep = "\n")

    # sign request using local private key
    private_key <- openssl::read_key(
      openssl::base64_decode(authInfo$private_key),
      der = TRUE
    )

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

rfc2616Date <- function(time = Sys.time()) {
  # set locale to POSIX/C to ensure ASCII date
  old <- Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old))

  strftime(Sys.time(), "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
}

# Helpers -----------------------------------------------------------------

userAgent <- function() {
  paste("rsconnect", packageVersion("rsconnect"), sep = "/")
}

parseHttpHeader <- function(header) {
  split <- strsplit(header, ": ")[[1]]
  if (length(split) == 2) {
    return(list(name = split[1], value = split[2]))
  } else {
    return(NULL)
  }
}

parseHttpStatusCode <- function(statusLine) {
  # extract status code; needs to deal with HTTP/1.0, HTTP/1.1, and HTTP/2
  statusCode <- regexExtract("HTTP/[0-9]+\\.?[0-9]* ([0-9]+).*", statusLine)
  if (is.null(statusCode)) {
    return(-1)
  } else {
    return(as.integer(statusCode))
  }
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
    if (nzchar(resp) == 0) {
      break()
    }

    header <- parseHttpHeader(resp)
    if (!is.null(header)) {
      name <- tolower(header$name)
      if (name == "content-type") {
        contentType <- header$value
      }
      if (name == "content-length") {
        contentLength <- as.integer(header$value)
      }
      if (name == "location") {
        location <- header$value
      }
      if (name == "set-cookie") {
        setCookies <- c(setCookies, header$value)
      }
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
    content <- rawToChar(readBin(
      con = conn, what = "raw",
      n = contentLength
    ))
  }

  # emit JSON trace if requested
  if (httpTraceJson() && identical(contentType, "application/json")) {
    cat(paste0(">> ", content, "\n"))
  }

  # return list
  list(
    req = request,
    status = statusCode,
    location = location,
    contentType = contentType,
    content = content
  )
}
