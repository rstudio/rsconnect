

userAgent <- function() {
  paste("shinyapps", packageVersion("shinyapps"), sep="/")
}

parseHttpHeader <- function(header) {
  split <- strsplit(header, ": ")[[1]]
  if (length(split) == 2)
    return (list(name = split[1], value = split[2]))
  else
    return (NULL)
}

parseHttpStatusCode <- function(statusLine) {
  statusCode <- regexExtract("HTTP/[0-9]+\\.[0-9]+ ([0-9]+).*", statusLine)
  if (is.null(statusCode))
    return (-1)
  else
    return (as.integer(statusCode))
}

readHttpResponse <- function(path, conn) {
  # read status code
  resp <- readLines(conn, 1)
  statusCode <- parseHttpStatusCode(resp[1])
  
  # read response headers
  contentLength <- NULL
  location <- NULL
  repeat {
    resp <- readLines(conn, 1)
    if (nzchar(resp) == 0)
      break()
    
    header <- parseHttpHeader(resp)
    if (!is.null(header))
    {
      if (identical(header$name, "Content-Type"))
        contentType <- header$value
      if (identical(header$name, "Content-Length"))
        contentLength <- as.integer(header$value)
      if (identical(header$name, "Location"))
        location <- header$value
    }
  }
  
  # read the response content
  content <- rawToChar(readBin(conn, what = 'raw', n=contentLength))
  
  # return list
  list(path = path,
       status = statusCode,
       location = location,
       contentType = contentType,
       content = content)
}


# internal sockets implementation of upload
httpInsecure <- function(host,
                         method,
                         path,
                         headers,
                         contentType = NULL,
                         file = NULL) {
  
  if (!is.null(file) && is.null(contentType))
    stop("You must specify a contentType for the specified file")
  
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
  for (name in names(headers))
  {
    request <- c(request, 
                 paste(name, ": ", headers[[name]], "\r\n", sep=""))
  }
  request <- c(request, "\r\n")
  
  # open socket connection
  conn <- socketConnection(host=host,
                           port=80,
                           open="w+b",
                           blocking=TRUE)
  on.exit(close(conn))
  
  # write the request header and file payload
  writeBin(charToRaw(paste(request,collapse="")), conn, size=1)
  if (!is.null(file)) {
    writeBin(fileContents, conn, size=1)
  }
  
  # read the response
  readHttpResponse(path, conn)      
}

httpGetInsecure <- function(host,
                            path,
                            headers) {
  httpInsecure(host, "GET", path, headers)
}

httpPostInsecure <- function(host,
                             path,
                             headers,
                             contentType,
                             file) {
  httpInsecure(host, "POST", path, headers, contentType, file)
}


httpCurl <- function(host,
                     method,
                     path,
                     headers,
                     contentType = NULL,
                     file = NULL) {  
  
  if (!is.null(file) && is.null(contentType))
    stop("You must specify a contentType for the specified file")
  
  if (!is.null(file)) 
    fileLength <- file.info(file)$size
  
  extraHeaders <- character()
  for (header in names(headers))
  {
    extraHeaders <- paste(extraHeaders, "--header")
    extraHeaders <- paste(extraHeaders,  
                          paste('"', header,": ",headers[[header]], '"', sep=""))
  }
  
  outputFile <- tempfile()
  
  command <- paste("curl", 
                   "-i",
                   "-X", 
                   method);
  
  if (!is.null(file)) {
    command <- paste(command,
                     "--data-binary",
                     shQuote(paste("@", file, sep="")),
                     "--header", paste("Content-Type:",contentType, sep=""),
                     "--header", paste("Content-Length:", fileLength, sep=""))
  }
    
  command <- paste(command,
                   extraHeaders,
                   "--header", "Expect:",
                   "--user-agent", userAgent(), 
                   "--silent",
                   "--show-error",
                   "-o", shQuote(outputFile),
                   paste("https://", host, path, sep=""))
  
  result <- system(command)
  
  if (result == 0) {
    fileConn <- file(outputFile, "rb")
    on.exit(close(fileConn))
    readHttpResponse(path, fileConn)
  } else {
    stop(paste("Curl request failed (curl error", result, "occurred)"))
  }
}

httpGetCurl <- function(host,
                        path,
                        headers) {
  httpCurl(host, "GET", path, headers)
}

httpPostCurl <- function(host,
                         path,
                         headers,
                         contentType,
                         file) {  
  httpCurl(host, "POST", path, headers, contentType, file)
}

httpRCurl <- function(host,
                      method,
                      path,
                      headers,
                      contentType = NULL,
                      file = NULL) {
  
  if (!is.null(file) && is.null(contentType))
    stop("You must specify a contentType for the specified file")
  
  # build url
  url <- paste("https://", host, path, sep="")
  
  # create upload params if necessary
  if (!is.null(file)) {
    params <- list(file = RCurl::fileUpload(filename = file,
                                            contentType = contentType))
  }
  
  # establish options
  options <- RCurl::curlOptions(url)
  options$followlocation <- 1L
  options$maxredirs <- 10L
  options$encoding <- "gzip"
  # TODO: verify SSL peers on windows (debug)
  options$ssl.verifypeer <- !identical(.Platform$OS.type, "windows")
  options$cainfo <- system.file("CurlSSL/cacert.pem", package = "RCurl")
  headerGatherer <- RCurl::basicHeaderGatherer()
  options$headerfunction <- headerGatherer$update
  textGatherer <- RCurl::basicTextGatherer()
  if (!is.null(file))
    options$writefunction <- textGatherer$update
  
  # add extra headers
  extraHeaders <- as.character(headers)
  names(extraHeaders) <- names(headers)
  options$httpheader <- extraHeaders
  
  # make the request
  if (!is.null(file)) {
    RCurl::postForm(url,
                    .params = params,
                    .opts = options,
                    cainfo = options$cainfo,
                    useragent = userAgent())
  } else {
    RCurl::getURL(url, 
                  .opts = options,
                  write = textGatherer,
                  cainfo = options$cainfo,
                  useragent = userAgent())
  }
  
  # return list
  headers <- headerGatherer$value()
  if ("Location" %in% names(headers))
    location <- headers[["Location"]]
  else
    location <- NULL
  list(status = as.integer(headers[["status"]]),
       location = location,
       contentType = headers[["Content-Type"]],
       content = textGatherer$value())
}


httpGetRCurl <- function(host,
                         path,
                         headers) {
  httpRCurl(host, "GET", path, headers)
}

httpPostRCurl <- function(host,
                          path,
                          headers,
                          contentType,
                          file) {  
  httpRCurl(host, "POST", path, headers, contentType, file)
}


httpFunction <- function() {
  
  httpType <- getOption("shinyapps.http", "auto")
  httpFunction <- NULL
  if (identical("auto", httpType)) {
    if (nzchar(Sys.which("curl")))
      httpFunction <- httpCurl
    else
      httpFunction <- httpRCurl
  } else if (identical("curl",  httpType)) {
    httpFunction <- httpCurl
  } else if (identical("rcurl", httpType)) {
    httpFunction <- httpRCurl
  } else if (identical("insecure", httpType)) {
    httpFunction <- httpInsecure
  } else {
    stop(paste("Invalid http option specified:",httpType,
               ". Valid values are auto, curl, rcurl, and insecure."))
  }
}

POST <- function(authInfo, 
                 path, 
                 contentType, 
                 file, 
                 headers = list()) {
  
  # get signature headers and append them
  sigHeaders <- signatureHeaders(authInfo, "POST", path, file)
  headers <- append(headers, sigHeaders)
  
  # perform POST
  http <- httpFunction()
  http("api.shinyapps.io", "POST", path, headers, contentType, file)
}

GET <- function(authInfo,
                path, 
                headers = list()) {
    
  # get signature headers and append them
  sigHeaders <- signatureHeaders(authInfo, "GET", path, NULL)
  headers <- append(headers, sigHeaders)
   
  # perform GET
  http <- httpFunction()
  http("api.shinyapps.io", "GET", path, headers)
}

signatureHeaders <- function(authInfo, method, path, file) {
  
  # headers to return
  headers <- list()
  
  # remove query string from path if necessary
  path <- strsplit(path, "?", fixed = TRUE)[[1]]
  
  # generate date
  date <- strftime(Sys.time(), "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
  
  # generate contents hash
  if (!is.null(file))
    md5 <- digest::digest(file, algo="md5", file=TRUE)
  else
    md5 <- digest::digest("", algo="md5", serialize=FALSE)
  
  # build cannonical request
  cannonicalRequest <- paste(method, path, date, md5, sep="\n")
  
  # sign request
  decodedSecret <- RCurl::base64Decode(authInfo$secret)
  hmac <- digest::hmac(decodedSecret, cannonicalRequest, algo="sha256")                
  signature <- paste(RCurl::base64Encode(hmac), "; version=1", sep="")
  
  # return headers
  headers$Date <- date
  headers$`X-Auth-Token` <- authInfo$token
  headers$`X-Auth-Signature` <- signature
  headers$`X-Content-Checksum` <- md5
  headers
}


