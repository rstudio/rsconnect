
# return a list of functions that can be used to interact with connect
connectClient <- function(authInfo) {

  list(

    status = function() {
      handleResponse(GET(authInfo,  "/internal/status"))
    },

    currentUser = function() {
      handleResponse(GET(authInfo, "/users/current/"))
    },

    accountsForUser = function(userId) {
      path <- "/accounts/"
      query <- ""
      listRequest(authInfo, path, query, "accounts")
    },

    listApplications = function(accountId, filters = NULL) {
      if (is.null(filters)) {
        filters <- vector()
      }
      path <- "/applications/"
      filters <- c(filterQuery("account_id", accountId), filters)
      query <- paste(filters, collapse="&")
      listRequest(authInfo, path, query, "applications")
    },

    getApplicationInfo = function(appId) {

    },

    createApplication = function(name, template, accountId) {
      json <- list()
      json$name <- name
      json$template <- template
      json$account <- as.numeric(accountId)
      handleResponse(POST_JSON(authInfo, "/applications/", json))
    },

    configureApplication = function(applicationId, propertyName, propertyValue) {
      path <- paste("/applications/", applicationId, "/properties/", propertyName, sep="")
      v <- list()
      v$value <- propertyValue
      handleResponse(PUT_JSON(authInfo, path, v))
    },

    uploadApplication = function(applicationId, bundlePath) {
      path <- paste("/applications/", applicationId, "/upload", sep="")
      handleResponse(POST(authInfo, path, "application/x-gzip", bundlePath))
    },

    deployApplication = function(applicationId, bundleId=NULL) {
      path <- paste("/applications/", applicationId, "/deploy", sep="")
      json <- list()
      json$bundle <- as.numeric(bundleId)
      handleResponse(POST_JSON(authInfo, path, json))
    },

    terminateApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, "/terminate", sep="")
      handleResponse(POST_JSON(authInfo, path, list()))
    },

    scaleApplication = function(applicationId, instances) {
      path <- paste("/applications/", applicationId, "/scale", sep="")
      json <- list()
      json$instance_count <- instances
      handleResponse(POST_JSON(authInfo, path, json))
    },

    listTasks = function(accountId, filters = NULL) {
      if (is.null(filters)) {
        filters <- vector()
      }
      path <- "/tasks/"
      filters <- c(filterQuery("account_id", accountId), filters)
      query <- paste(filters, collapse="&")
      listRequest(authInfo, path, query, "tasks", max=100)
    },

    getTaskInfo = function(taskId) {
      path <- paste("/tasks/", taskId, sep="")
      handleResponse(GET(authInfo, path))
    },

    getTaskLogs = function(taskId) {
      path <- paste("/tasks/", taskId, "/logs/", sep="")
      handleResponse(GET(authInfo, path))
    },

    waitForTask = function(taskId, quiet = FALSE) {

      if (!quiet) {
        cat("Waiting for task: ", taskId, "\n", sep="")
      }

      path <- paste("/tasks/", taskId, sep="")

      lastStatus <- NULL
      while(TRUE) {

        # check status
        status <- handleResponse(GET(authInfo, path))

        # display status to the user if it changed
        if (!identical(lastStatus, status$description)) {
          if (!quiet)
            cat("  ", status$status, ": ", status$description, "\n", sep="")
          lastStatus <- status$description
        }

        # are we finished? (note: this codepath is the only way to exit this function)
        if (status$finished) {
          if (identical(status$status, "success")) {
            return (NULL)
          } else {
            # always show task log on error
            hr("Begin Log")
            taskLog(taskId, authInfo$name, output="stderr")
            hr("End Log")
            stop(status$error, call. = FALSE)
          }
        }

        # wait for 1 second before polling again
        Sys.sleep(1)
      }
    }
  )
}

listRequest = function(authInfo, path, query, listName, page = 100, max=NULL) {

  # accumulate multiple pages of results
  offset <- 0
  results <- list()

  while(TRUE) {

    # add query params
    pathWithQuery <- paste(path, "?", query,
                           "&count=", page,
                           "&offset=", offset,
                           sep="")

    # make request and append the results
    response <- handleResponse(GET(authInfo, pathWithQuery))
    results <- append(results, response[[listName]])

    # update the offset
    offset <- offset + response$count

    # get all results if no max was specified
    if (is.null(max)) {
      max = response$total
    }

    # exit if we've got them all
    if (length(results) >= response$total || length(results) >= max)
      break
  }

  return(results)
}

handleResponse <- function(response, jsonFilter = NULL) {

  # function to report errors
  reportError <- function(msg) {
    stop(paste(response$path, response$status, "-", msg), call. = FALSE)
  }

  # json responses
  if (isContentType(response, "application/json")) {

    json <- RJSONIO::fromJSON(response$content, simplify = FALSE)

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
    if (response$status %in% 200:399)
      body
    else if (!is.null(body))
      reportError(body)
    else
      reportError(response$content)
  }

  # otherwise just dump the whole thing
  else {
    if (response$status %in% 200:399)
      response$content
    else
      reportError(response$content)
  }
}

filterQuery <- function(param, value, operator = NULL) {
  if (is.null(operator)) {
    op <- ":"
  } else {
    op <- paste(":", operator, ":", sep="")
  }
  q <- paste("filter=", param, op, value, sep="")
  return(q)
}

isContentType <- function(response, contentType) {
  grepl(contentType, response$contentType, fixed = TRUE)
}
