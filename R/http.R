

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
httpInternal <- function(host,
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
  
  # output request if in verbose mode
  if (httpVerbose())
    cat(request)
    
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
  response <- readHttpResponse(path, conn)
  
  # print if in verbose mode
  if (httpVerbose())
    print(response)
  
  # return it
  response
}

httpGetInternal <- function(host,
                            path,
                            headers) {
  httpInternal(host, "GET", path, headers)
}

httpPostInternal <- function(host,
                             path,
                             headers,
                             contentType,
                             file) {
  httpInternal(host, "POST", path, headers, contentType, file)
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
  
  if (httpVerbose())
    command <- paste(command, "-v")
  
  if (!is.null(file)) {
    command <- paste(command,
                     "--data-binary",
                     shQuote(paste("@", file, sep="")),
                     "--header", paste('"' ,"Content-Type: ",contentType, '"', sep=""),
                     "--header", paste('"', "Content-Length: ", fileLength, '"', sep=""))
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
  
  # read file in binary mode
  if (!is.null(file)) {
    fileLength <- file.info(file)$size
    fileContents <- readBin(file, what="raw", n=fileLength)
    headers$`Content-Type` <- contentType
  }
  
  # establish options
  options <- RCurl::curlOptions(url)
  options$useragent <- userAgent()
  options$ssl.verifypeer <- TRUE
  # Cert from: http://curl.haxx.se/docs/caextract.html
  options$cainfo <- system.file("cert/cacert.pem", package = "shinyapps")
  headerGatherer <- RCurl::basicHeaderGatherer()
  options$headerfunction <- headerGatherer$update
  textGatherer <- RCurl::basicTextGatherer()
  if (!is.null(file))
    options$writefunction <- textGatherer$update
  
  # verbose if requested
  if (httpVerbose())
    options$verbose <- TRUE
  
  # add extra headers
  extraHeaders <- as.character(headers)
  names(extraHeaders) <- names(headers)
  options$httpheader <- extraHeaders
  
  # make the request
  if (!is.null(file)) {
    RCurl::curlPerform(url = url,
                       .opts = options,
                       customrequest = method,
                       readfunction = fileContents,
                       infilesize = fileLength,
                       upload = TRUE)
  } else {
    RCurl::getURL(url, 
                  .opts = options,
                  write = textGatherer)
  }
  
  # return list
  headers <- headerGatherer$value()
  if ("Location" %in% names(headers))
    location <- headers[["Location"]]
  else
    location <- NULL
  list(path = path,
       status = as.integer(headers[["status"]]),
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

httpVerbose <- function() {
  getOption("shinyapps.http.verbose", FALSE)
}

httpFunction <- function() {
  
  httpType <- getOption("shinyapps.http", "rcurl")
  if (identical("rcurl", httpType))
    httpRCurl
  else if (identical("curl",  httpType))
    httpCurl
  else if (identical("internal", httpType))
    httpInternal
  else
    stop(paste("Invalid http option specified:",httpType,
               ". Valid values are rcurl, curl, and internal"))
}

POST_JSON <- function(authInfo, path, json, headers = list()) {
  POST(authInfo,
       path,
       "application/json",
       content = RJSONIO::toJSON(json, pretty = TRUE),
       headers = headers)
}

POST <- function(authInfo, 
                 path, 
                 contentType, 
                 file = NULL,
                 content = NULL,
                 headers = list()) {
  httpWithBody(authInfo, "POST", path, contentType, file, content, headers)
}

PUT_JSON <- function(authInfo, path, json, headers = list()) {
  PUT(authInfo,
      path,
      "application/json",
      content = RJSONIO::toJSON(json, pretty = TRUE),
      headers = headers)
}

PUT <- function(authInfo,
                path, 
                contentType, 
                file = NULL,
                content = NULL,
                headers = list()) {
  httpWithBody(authInfo, "PUT", path, contentType, file, content, headers)
}


httpWithBody <- function(authInfo, 
                         method,
                         path, 
                         contentType, 
                         file = NULL,
                         content = NULL,
                         headers = list()) {
  
  if ((is.null(file) && is.null(content)))
    stop("You must specify either the file or content parameter.")  
  if ((!is.null(file) && !is.null(content))) 
    stop("You must specify either the file or content parameter but not both.")
          
  # get signature headers and append them
  sigHeaders <- signatureHeaders(authInfo, "POST", path, file)
  headers <- append(headers, sigHeaders)
  
  # if we have content then write it to a temp file before posting
  file <- tempfile()
  writeChar(content, file,  eos = NULL, useBytes=TRUE)
  
  # perform POST
  http <- httpFunction()
  http("api.shinyapps.io", method, path, headers, contentType, file)
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


