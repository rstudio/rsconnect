clientForAccount <- function(account) {

  # determine appropriate server information for account
  if (isCloudServer(account$server)) {
    constructor <- lucidClientForAccount(account)
  } else {
    serverInfo <- serverInfo(account$server)
    account$certificate <- serverInfo$certificate
    connectClient(serverInfo$url, account)
  }
}

lucidClientForAccount <- function(account) {
  authInfo <- account

  # determine appropriate server information for account
  if (account$server == cloudServerInfo(account$server)$name) {
    serverInfo <- cloudServerInfo(account$server)
    constructor <- cloudClient
  } else {
    serverInfo <- shinyappsServerInfo()
    constructor <- shinyAppsClient
  }

  # promote certificate into auth info
  authInfo$certificate <- serverInfo$certificate
  serverUrl <- parseHttpUrl(serverInfo$url)

  constructor(serverUrl, authInfo)
}

listRequest <- function(service, authInfo, path, query, listName, page = 100,
                       max = NULL) {

  # accumulate multiple pages of results
  offset <- 0
  results <- list()

  while (TRUE) {

    # add query params
    queryWithList <- paste(query, "&count=", page, "&offset=", offset, sep = "")

    # make request and append the results
    response <- handleResponse(GET(service, authInfo, path, queryWithList))
    results <- append(results, response[[listName]])

    # update the offset
    offset <- offset + response$count

    # get all results if no max was specified
    if (is.null(max)) {
      max <- response$total
    }

    # exit if we've got them all
    if (length(results) >= response$total || length(results) >= max)
      break
  }

  return(results)
}

filterQuery <- function(param, value, operator = NULL) {
  if (is.null(operator)) {
    op <- ":"
  } else {
    op <- paste(":", operator, ":", sep = "")
  }
  q <- paste("filter=", param, op, value, sep = "")
  return(q)
}

isContentType <- function(response, contentType) {
  grepl(contentType, response$contentType, fixed = TRUE)
}

uploadCloudBundle <- function(client,
                              application_id,
                              bundlePath,
                              verbose = FALSE) {
  # Step 1. Create presigned URL and register pending bundle.
  bundleSize <- file.info(bundlePath)$size
  bundle <- client$createBundle(
    application_id,
    content_type = "application/x-tar",
    content_length = bundleSize,
    checksum = fileMD5.as.string(bundlePath)
  )

  # Step 2. Upload Bundle to presigned URL
  logger <- verboseLogger(verbose)
  logger("Starting upload now")
  if (!uploadBundle(bundle, bundleSize, bundlePath)) {
    stop("Could not upload file.")
  }
  logger("Upload complete")

  # Step 3. Upload revise bundle status.
  response <- client$updateBundleStatus(bundle$id, status = "ready")

  # Step 4. Retrieve updated bundle post status change
  client$getBundle(bundle$id)
}

uploadBundle <- function(bundle, bundleSize, bundlePath) {

  presigned_service <- parseHttpUrl(bundle$presigned_url)

  headers <- list()
  headers$`Content-Type` <-  "application/x-tar"
  headers$`Content-Length` <-  bundleSize

  # AWS requires a base64 encoded hash
  headers$`Content-MD5` <-  bundle$presigned_checksum

  # AWS seems very sensitive to additional headers (likely becauseit was not included and signed
  # for when the presigned link was created). So the lower level library is used here.
  http <- httpFunction()
  response <- http(
    presigned_service$protocol,
    presigned_service$host,
    presigned_service$port,
    "PUT",
    presigned_service$path,
    headers,
    headers$`Content-Type`,
    bundlePath
  )

  response$status == 200
}

handleResponse <- function(response, jsonFilter = NULL) {

  # function to report errors
  reportError <- function(msg) {
    stop("HTTP ", response$status, "\n",
         response$req$method, " ",  response$req$protocol, "://",
         response$req$host, response$req$port, response$req$path, "\n",
         msg, call. = FALSE)
  }

  # json responses
  if (isContentType(response, "application/json")) {

    if (nzchar(response$content)) {
      json <- jsonlite::fromJSON(response$content, simplifyVector = FALSE)
    } else {
      json <- list()
    }

    if (response$status %in% 200:399)
      if (!is.null(jsonFilter))
        jsonFilter(json)
      else
        json
    else if (!is.null(json$error))
      reportError(json$error)
    else
      reportError(paste("Unexpected json response:", response$content))
  }

  # for html responses we can attempt to extract the body
  else if (isContentType(response, "text/html")) {

    body <- regexExtract(".*?<body>(.*?)</body>.*", response$content)
    if (response$status >= 200 && response$status < 400) {
      # Good response, return the body if we have one, or the content if not
      if (!is.null(body)) {
        body
      } else{
        response$content
      }
    } else {
      # Error response
      if (!is.null(body))
        reportError(body)
      else
        reportError(response$content)
    }
  }

  # otherwise just dump the whole thing
  else {
    if (response$status %in% 200:399)
      response$content
    else
      reportError(response$content)
  }
}
