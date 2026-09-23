clientForAccount <- function(account) {
  serverInfo <- serverInfo(account$server)
  account$certificate <- serverInfo$certificate
  serverUrl <- parseHttpUrl(serverInfo$url)

  if (isShinyappsServer(account$server)) {
    shinyAppsClient(serverUrl, account)
  } else if (isPositConnectCloudServer(account$server)) {
    connectCloudClient(serverUrl, account)
  } else if (isSPCSServer(account$server)) {
    account$snowflakeToken <- getSnowflakeAuthToken(
      serverInfo$url,
      account$snowflakeConnectionName
    )
    connectClient(serverUrl, account)
  } else {
    # Standard Connect server - try identity federation if no credentials
    if (hasNoCredentials(account)) {
      ephemeralApiKey <- attemptIdentityFederation(serverInfo$url)
      if (!is.null(ephemeralApiKey)) {
        account$apiKey <- ephemeralApiKey
      }
    }
    connectClient(serverUrl, account)
  }
}

hasNoCredentials <- function(account) {
  is.null(account$apiKey) &&
    is.null(account$token) &&
    is.null(account$secret) &&
    is.null(account$private_key) &&
    is.null(account$accessToken)
}

# Appropriate when the list API includes "count" and "total" fields in the response JSON and the API
# supports pagination with the query arguments count=PAGE_SIZE&offset=STARTING_POINT.
listRequest <- function(
  service,
  authInfo,
  path,
  query,
  listName,
  page = 100,
  max = NULL
) {
  # accumulate multiple pages of results
  offset <- 0
  results <- list()

  repeat {
    # add query params
    queryWithList <- paste(query, "&count=", page, "&offset=", offset, sep = "")

    # make request and append the results
    response <- GET(service, authInfo, path, queryWithList)
    results <- append(results, response[[listName]])

    # update the offset
    offset <- offset + response$count

    # get all results if no max was specified
    if (is.null(max)) {
      max <- response$total
    }

    # exit if we've got them all
    if (length(results) >= response$total || length(results) >= max) {
      break
    }
  }

  return(results)
}

# /__api__/applications response with { applications: [], count: M, total: N, continuation: "CONTINUATION" }
# To paginate, use the query arguments cont=CONTINUATION&start=START&count=MAX
listApplicationsRequest <- function(
  service,
  authInfo,
  path,
  query,
  listName,
  page = 100,
  max = NULL
) {
  # accumulate multiple pages of results
  start <- 0
  cont <- ""
  results <- list()

  repeat {
    # add query params
    queryWithList <- paste(
      query,
      "&count=",
      page,
      "&start=",
      start,
      "&cont=",
      cont,
      sep = ""
    )

    # make request and append the results
    response <- GET(service, authInfo, path, queryWithList)
    results <- append(results, response[[listName]])

    # update the starting point for the next request
    start <- start + response$count
    cont <- response$continuation

    # get all results if no max was specified
    if (is.null(max)) {
      max <- response$total
    }

    # exit if we've got them all
    if (length(results) >= response$total || length(results) >= max) {
      break
    }
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

isContentType <- function(x, contentType) {
  grepl(contentType, x, fixed = TRUE)
}
