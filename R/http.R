userAgent <- function() {
  paste("shinyapps", packageVersion("shinyapps"), sep="/")
}

serviceUrl <- function() {
  parseHttpUrl(getOption("shinyapps.service_url", 
                         "https://api.shinyapps.io/v1"))
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
httpInternal <- function(protocol,
                         host,
                         port,
                         method,
                         path,
                         headers,
                         contentType = NULL,
                         file = NULL, 
                         writer = NULL) {
  
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
  time <- system.time(gcFirst=FALSE, {
    conn <- socketConnection(host=host,
                             port=as.integer(port),
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
  })
  httpTrace(method, path, time)
  
  # print if in verbose mode
  if (httpVerbose())
    print(response)
  
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
                     writer = NULL) {  
  
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
    
  # add prefix to port if necessary
  if (nzchar(port))
    port <- paste(":", port, sep="")
  
  command <- paste(command,
                   extraHeaders,
                   "--header", "Expect:",
                   "--user-agent", userAgent(), 
                   "--silent",
                   "--show-error",
                   "-o", shQuote(outputFile),
                   paste(protocol, "://", host, port, path, sep=""))
  
  result <- NULL
  time <- system.time(gcFirst = FALSE, {  
    result <- system(command)
  })
  httpTrace(method, path, time)
  
  if (result == 0) {
    fileConn <- file(outputFile, "rb")
    on.exit(close(fileConn))
    readHttpResponse(path, fileConn)
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
                      writer = NULL) {
  
  if (!is.null(file) && is.null(contentType))
    stop("You must specify a contentType for the specified file")
  
  # add prefix to port if necessary
  if (nzchar(port))
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
  options$ssl.verifypeer <- TRUE
  # Cert from: http://curl.haxx.se/docs/caextract.html
  options$cainfo <- system.file("cert/cacert.pem", package = "shinyapps")
  headerGatherer <- RCurl::basicHeaderGatherer()
  options$headerfunction <- headerGatherer$update
  textGatherer <- if (is.null(writer)) 
      RCurl::basicTextGatherer()
    else
      writer
  
  # when using a custom output writer, add a progress check so we can 
  # propagate interrupts, and wait a long time (for streaming)
  if (!is.null(writer)) {
    options$noprogress <- FALSE
    options$progressfunction <- writer$progress
    options$timeout <- 9999999
  }
  
  # verbose if requested
  if (httpVerbose())
    options$verbose <- TRUE
  
  # add extra headers
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
        return
      # bubble remaining errors through  
      else
        stop(e)
    }))
  httpTrace(method, path, time)
  
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

httpVerbose <- function() {
  getOption("shinyapps.http.verbose", FALSE)
}

httpTrace <- function(method, path, time) {
  if (getOption("shinyapps.http.trace", FALSE)) {
    cat(method, " ", path, " ", as.integer(time[['elapsed']]*1000), "ms\n", 
        sep="")
  }
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

POST_JSON <- function(authInfo, 
                      path, 
                      json, 
                      query = NULL, 
                      headers = list()) {
  POST(authInfo,
       path,
       query,
       "application/json",
       content = RJSONIO::toJSON(json, pretty = TRUE, digits=30),
       headers = headers)
}

PUT_JSON <- function(authInfo, 
                     path, 
                     json, 
                     query = NULL,
                     headers = list()) {
  PUT(authInfo,
      path,
      query,
      "application/json",
      content = RJSONIO::toJSON(json, pretty = TRUE, digits=30),
      headers = headers)
}

POST <- function(authInfo, 
                 path, 
                 query = NULL, 
                 contentType = NULL, 
                 file = NULL,
                 content = NULL,
                 headers = list()) {
  httpRequestWithBody(authInfo, "POST", path, query, contentType, file, content, headers)
}

PUT <- function(authInfo,
                path, 
                query = NULL,
                contentType = NULL, 
                file = NULL,
                content = NULL,
                headers = list()) {
  httpRequestWithBody(authInfo, "PUT", path, query, contentType, file, content, headers)
}

GET <- function(authInfo,
                path, 
                query = NULL,
                headers = list(),
                writer = NULL) {
  httpRequest(authInfo, "GET", path, query, headers, writer)
}

DELETE <- function(authInfo,
                   path, 
                   query = NULL,
                   headers = list(),
                   writer = NULL) {
  httpRequest(authInfo, "DELETE", path, query, headers, writer)
}

httpRequestWithBody <- function(authInfo, 
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
          
  # get the service url
  service <- serviceUrl()
  
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

  # get signature headers and append them
  sigHeaders <- signatureHeaders(authInfo, method, url, file)
  headers <- append(headers, sigHeaders)
  
  # perform request 
  http <- httpFunction()
  http(service$protocol,
       service$host,
       service$port,
       method, 
       url,
       headers, 
       contentType, 
       file)
}

httpRequest <- function(authInfo,
                        method,
                        path, 
                        query,
                        headers = list(),
                        writer = NULL) {
    
  # get the service url
  service <- serviceUrl()
  
  # prepend the service path
  url <- paste(service$path, path, sep="")
  
  # append the query
  if (!is.null(query)) {
    # URL encode query args
    query <- utils::URLencode(query)
    url <- paste(url, "?", query, sep="")
  }
  
  # get signature headers and append them
  sigHeaders <- signatureHeaders(authInfo, method, url, NULL)
  headers <- append(headers, sigHeaders)
   
  # perform GET
  http <- httpFunction()
  http(service$protocol,
       service$host,
       service$port,
       method, 
       url,
       headers, 
       writer = writer)
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

signatureHeaders <- function(authInfo, method, path, file) {
  
  # headers to return
  headers <- list()
  
  # remove query string from path if necessary
  path <- strsplit(path, "?", fixed = TRUE)[[1]][[1]]

  # generate date
  date <- rfc2616Date()

  # generate contents hash
  if (!is.null(file))
    md5 <- digest::digest(file, algo="md5", file=TRUE)
  else
    md5 <- digest::digest("", algo="md5", serialize=FALSE)
  
  # build cannonical request
  cannonicalRequest <- paste(method, path, date, md5, sep="\n")
  
  # sign request
  decodedSecret <- RCurl::base64Decode(authInfo$secret, mode="raw")
  hmac <- digest::hmac(decodedSecret, cannonicalRequest, algo="sha256")                
  signature <- paste(RCurl::base64Encode(hmac), "; version=1", sep="")
  
  # return headers
  headers$Date <- date
  headers$`X-Auth-Token` <- authInfo$token
  headers$`X-Auth-Signature` <- signature
  headers$`X-Content-Checksum` <- md5
  headers
}
