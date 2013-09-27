

regexExtract <- function(re, input) {
  match <- regexec(re, input)
  matchLoc <- match[1][[1]]
  if (length(matchLoc) > 1) {
    matchLen <-attributes(matchLoc)$match.length
    return (substr(input, matchLoc[2], matchLoc[2] + matchLen[2]-1))
  }
  else {
    return (NULL)
  }
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

readHttpResponse <- function(conn) {
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
  list(status = statusCode,
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
  
  # read file in binary mode
  if (identical(method, "POST")) {
    fileLength <- file.info(file)$size
    fileContents <- readBin(file, what="raw", n=fileLength)
  }
 
  # build http request
  request <- NULL
  request <- c(request, paste(method, " ", path, " HTTP/1.1\r\n", sep=""))
  request <- c(request, "User-Agent: shinyapps\r\n")
  request <- c(request, "Host: ", host, "\r\n", sep="")
  request <- c(request, "Accept: */*\r\n")
  if (identical(method, "POST")) {
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
  if (identical(method, "POST")) {
    writeBin(fileContents, conn, size=1)
  }
  
  # read the response
  readHttpResponse(conn)      
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
  
  if (identical(method, "POST")) 
    fileLength <- file.info(file)$size
  
  extraHeaders <- character()
  for (header in names(headers))
  {
    extraHeaders <- paste(extraHeaders, "--header")
    extraHeaders <- paste(extraHeaders,  
                          paste(header,":",headers[[header]], sep=""))
  }
  
  outputFile <- tempfile()
  
  command <- paste("curl", 
                   "-i",
                   "-X", 
                   method);
  
  if (identical(method, "POST")) {
    command <- paste(command,
                     "--data-binary",
                     shQuote(paste("@", file, sep="")),
                     "--header", paste("Content-Type:",contentType, sep=""),
                     "--header", paste("Content-Length:", fileLength, sep=""))
  }
    
  command <- paste(command,
                   extraHeaders,
                   "--header", "Expect:",
                   "--silent",
                   "--show-error",
                   "-o", shQuote(outputFile),
                   paste("https://", host, path, sep=""))
  
  result <- system(command)
  
  if (result == 0) {
    fileConn <- file(outputFile, "rb")
    on.exit(close(fileConn))
    readHttpResponse(fileConn)
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


#' @importFrom RCurl curlOptions basicHeaderGatherer basicTextGatherer getURL
httpGetRCurl <- function(host,
                         path,
                         headers) {
  # url to post to
  url <- paste("https://", host, path, sep="")
   
  # use custom header and text gatherers
  options <- RCurl::curlOptions(url)
  headerGatherer <- RCurl::basicHeaderGatherer()
  options$headerfunction <- headerGatherer$update
  textGatherer <- RCurl::basicTextGatherer()
  
  # add extra headers
  extraHeaders <- as.character(headers)
  names(extraHeaders) <- names(headers)
  options$httpheader <- extraHeaders
  
  # do the get
  RCurl::getURL(url, 
                .opts = options,
                write = textGatherer,
                useragent = "shinyapps")
  
  # return list
  headers <- headerGatherer$value()
  if ("Location" %in% names(headers))
    location <- headers[["Location"]]
  else
    location <- NULL
  list(status = as.integer(headers[["status"]]),
       location = location,
       contentType <- headers[["Content-Type"]],
       content = textGatherer$value())
}

#' @importFrom RCurl fileUpload curlOptions basicHeaderGatherer 
#'                   basicTextGatherer postForm
httpPostRCurl <- function(host,
                          path,
                          headers,
                          contentType,
                          file) {
  
  # url to post to
  url <- paste("https://", host, path, sep="")
  
  # upload package file
  params <- list(file = RCurl::fileUpload(filename = file,
                                          contentType = contentType))
  
  # use custom header and text gatherers
  options <- RCurl::curlOptions(url)
  headerGatherer <- RCurl::basicHeaderGatherer()
  options$headerfunction <- headerGatherer$update
  textGatherer <- RCurl::basicTextGatherer()
  options$writefunction <- textGatherer$update
  
  # add extra headers
  extraHeaders <- as.character(headers)
  names(extraHeaders) <- names(headers)
  options$httpheader <- extraHeaders
  
  # post the form
  RCurl::postForm(url,
                  .params = params,
                  .opts = options,
                  useragent = "shinyapps")
  
  # return list
  headers <- headerGatherer$value()
  if ("Location" %in% names(headers))
    location <- headers[["Location"]]
  else
    location <- NULL
  list(status = as.integer(headers[["status"]]),
       location = location,
       contentType <- headers[["Content-Type"]],
       content = textGatherer$value())
}

httpFunction <- function(http, functions) {
  
  httpFunction <- NULL
  if (is.function(http)) {
    httpFunction <- http
  } else if (identical("rcurl", http)) {
    httpFunction <- functions$rcurl
  } else if (identical("curl",  http)) {
    httpFunction <- functions$curl
  } else if (identical("insecure", http)) {
    httpFunction <- functions$insecure
  } else {
    stop(paste("Invalid http connection type specified:",http,
               ". Valid values are rcurl, curl, and insecure."))
  }
}

httpPost <- function(http, path, contentType, file, headers = list()) {
  functions <- list()
  functions$curl <- httpPostCurl
  functions$rcurl <- httpPostRCurl
  functions$insecure <- httpPostInsecure
  postFunction <- httpFunction(http, functions)
  postFunction("api.shinyapps.io", path, headers, contentType, file)
}

httpGet <- function(http, path, headers = list()) {
  functions <- list()
  functions$curl <- httpGetCurl
  functions$rcurl <- httpGetRCurl
  functions$insecure <- httpGetInsecure
  getFunction <- httpFunction(http, functions)
  getFunction("api.shinyapps.io", path, headers)
}

uploadBundle <- function(http, file) {
  httpPost(http, "/bundle/upload", "application/x-compressed", file)
} 

serviceStatus <- function(http) {
  httpGet(http, "/internal/status")
}


