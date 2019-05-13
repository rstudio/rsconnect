# HTTP transport using the curl command-line utility. Useful on systems that have a working curl but
# not necessarily a working curl library, such as Windows 10.

httpCurl <- function(protocol,
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

  if (!is.null(contentFile))
    fileLength <- file.info(contentFile)$size

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

  if (!is.null(contentFile)) {
    command <- paste(command,
                     "--data-binary",
                     shQuote(paste("@", contentFile, sep="")),
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
  if (!is.null(contentFile) && httpTraceJson() &&
      identical(contentType, "application/json"))
  {
    fileLength <- file.info(contentFile)$size
    fileContents <- readBin(contentFile, what="raw", n=fileLength)
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


