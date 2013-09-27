

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
httpPostInternalCore <- function(host,
                                 port,
                                 path,
                                 headers,
                                 contentType,
                                 postFile) {
  
  # read file in binary mode
  fileLength <- file.info(postFile)$size
  fileContents <- readBin(postFile, what="raw", n=fileLength)
  
  # build http request
  request <- NULL
  request <- c(request, paste("POST ", path, " HTTP/1.1\r\n", sep=""))
  request <- c(request, "User-Agent: shinyapps\r\n")
  request <- c(request, "Host: ", host, "\r\n", sep="")
  request <- c(request, "Accept: */*\r\n")
  request <- c(request, paste("Content-Type: ", 
                              contentType, 
                              "\r\n", 
                              sep=""))
  request <- c(request, paste("Content-Length: ", 
                              fileLength, 
                              "\r\n", 
                              sep=""))
  for (name in names(headers))
  {
    request <- c(request, 
                 paste(name, ": ", headers[[name]], "\r\n", sep=""))
  }
  request <- c(request, "\r\n")
  
  # open socket connection
  conn <- socketConnection(host=host,
                           port=port,
                           open="w+b",
                           blocking=TRUE)
  on.exit(close(conn))
  
  # write the request header and file payload
  writeBin(charToRaw(paste(request,collapse="")), conn, size=1)
  writeBin(fileContents, conn, size=1)
  
  # read the response
  readHttpResponse(conn)      
}

httpPostInternal <- function(host,
                             path,
                             headers,
                             contentType,
                             postFile) {
  httpPostInternalCore(host, 80, path, headers, contentType, postFile)
}

httpsPostInternal <- function(host,
                              path,
                              headers,
                              contentType,
                              postFile) {
  httpPostInternalCore(host, 443, path, headers, contentType, postFile)
}

httpsPostCurl <- function(host,
                          path,
                          contentType,
                          headers,
                          postFile) {  
  
  fileLength <- file.info(postFile)$size
  
  extraHeaders <- character()
  for (header in names(headers))
  {
    extraHeaders <- paste(extraHeaders, "--header")
    extraHeaders <- paste(extraHeaders,  
                          paste(header,":",headers[[header]], sep=""))
  }
  
  outputFile <- tempfile()
  
  command <- paste("curl", 
                   "-X", 
                   "POST",
                   "--data-binary",
                   shQuote(paste("@", postFile, sep="")),
                   "-i",
                   "--header", paste("Content-Type:",contentType, sep=""),
                   "--header", paste("Content-Length:", fileLength, sep=""),
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
    stop(paste("Upload failed (curl error", result, "occurred)"))
  }
}


#' @importFrom RCurl fileUpload curlOptions basicHeaderGatherer 
#'                   basicTextGatherer postForm
httpsPostRCurl <- function(host,
                           path,
                           contentType,
                           headers,
                           postFile) {
  
  # url to post to
  url <- paste("https://", host, path, sep="")
  
  # upload package file
  params <- list(file = RCurl::fileUpload(filename = postFile,
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
  RCurl::postForm(paste("https://", host, path, sep=""),
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





