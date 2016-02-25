
# return a list of functions that can be used to interact with lucid
lucidClient <- function(service, authInfo) {
  service <- parseHttpUrl(service)

  list(

    status = function() {
      handleResponse(GET(service, authInfo,  "/internal/status"))
    },

    currentUser = function() {
      handleResponse(GET(service, authInfo, "/users/current/"))
    },

    accountsForUser = function(userId) {
      path <- "/accounts/"
      query <- ""
      listRequest(service, authInfo, path, query, "accounts")
    },

    getAccountUsage = function(accountId, usageType='hours', applicationId=NULL,
                               from=NULL, until=NULL, interval=NULL) {
      path <- paste("/accounts/", accountId, "/usage/", usageType, "/", sep="")
      query <- list()
      if (!is.null(applicationId))
        query$application=applicationId
      if (!is.null(from)) 
        query$from = from
      if (is.null(until)) 
        query$until = until
      if (is.null(interval))
        query$interval = interval
      handleResponse(GET(service, authInfo, path, queryString(query)))
    },

   listApplications = function(accountId, filters = list()) {
      path <- "/applications/"
      query <- paste(filterQuery(
        c("account_id", names(filters)),
        c(accountId, unname(filters))
      ), collapse = "&")
      listRequest(service, authInfo, path, query, "applications")
    },

    getApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, sep="")
      handleResponse(GET(service, authInfo, path))
    },

    getApplicationMetrics = function(applicationId, series, metrics, from=NULL, until=NULL, interval=NULL) {
      path <- paste("/applications/", applicationId, "/metrics/", series, "/", sep="")
      query <- list()
      m <- paste(lapply(metrics, function(x){paste("metric", urlEncode(x), sep="=")}), collapse = "&")
      if (!is.null(from))
        query$from = from
      if (is.null(until))
        query$until = until
      if (is.null(interval))
        query$interval = interval
      handleResponse(GET(service, authInfo, path, paste(m, queryString(query), sep="&")))
    },

    getLogs = function(applicationId, entries = 50, streaming = FALSE,
                       writer = NULL) {
      path <- paste("/applications/", applicationId, "/logs", sep="")
      query <- paste("count=", entries,
                     "&tail=", if (streaming) "1" else "0", sep="")
      handleResponse(GET(service, authInfo, path, query, writer = writer))
    },

    createApplication = function(name, template, accountId) {
      json <- list()
      json$name <- name
      json$template <- template
      json$account <- as.numeric(accountId)
      handleResponse(POST_JSON(service, authInfo, "/applications/", json))
    },

    listApplicationProperties = function(applicationId) {
      path <- paste("/applications/", applicationId, "/properties/", sep="")
      handleResponse(GET(authInfo, path))
    },

    setApplicationProperty = function(applicationId, propertyName,
                                      propertyValue, force=FALSE) {
      path <- paste("/applications/", applicationId, "/properties/",
                    propertyName, sep="")
      v <- list()
      v$value <- propertyValue
      query <- paste("force=", if (force) "1" else "0", sep="")
      handleResponse(PUT_JSON(authInfo, path, v, query))
    },

    unsetApplicationProperty = function(applicationId, propertyName,
                                        force=FALSE) {
      path <- paste("/applications/", applicationId, "/properties/",
                    propertyName, sep="")
      query <- paste("force=", if (force) "1" else "0", sep="")
      handleResponse(DELETE(service, authInfo, path, query))
    },

    uploadApplication = function(applicationId, bundlePath) {
      path <- paste("/applications/", applicationId, "/upload", sep="")
      handleResponse(POST(service,
                          authInfo,
                          path,
                          contentType="application/x-gzip",
                          file=bundlePath))
    },

    deployApplication = function(applicationId, bundleId=NULL) {
      path <- paste("/applications/", applicationId, "/deploy", sep="")
      json <- list()
      json$bundle <- as.numeric(bundleId)
      handleResponse(POST_JSON(service, authInfo, path, json))
    },

    terminateApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, "/terminate", sep="")
      handleResponse(POST_JSON(service, authInfo, path, list()))
    },

    inviteApplicationUser = function(applicationId, email,
                                     invite_email=NULL, invite_email_message=NULL) {
      path <- paste("/applications/", applicationId, "/authorization/users",
                    sep="")
      json <- list()
      json$email <- email
      if (!is.null(invite_email))
        json$invite_email=invite_email
      if (!is.null(invite_email_message))
        json$invite_email_message=invite_email_message
      handleResponse(POST_JSON(service, authInfo, path, json))
    },

    addApplicationUser = function(applicationId, userId) {
      path <- paste("/applications/", applicationId, "/authorization/users/",
                    userId, sep="")
      handleResponse(PUT(service, authInfo, path, NULL))
    },

    removeApplicationUser = function(applicationId, userId) {
      path <- paste("/applications/", applicationId, "/authorization/users/",
                    userId, sep="")
      handleResponse(DELETE(service, authInfo, path, NULL))
    },

    listApplicationAuthoization = function(applicationId) {
      path <- paste("/applications/", applicationId, "/authorization",
                    sep="")
      listRequest(service, authInfo, path, NULL, "authorization")
    },

    listApplicationUsers = function(applicationId) {
      path <- paste("/applications/", applicationId, "/authorization/users",
                    sep="")
      listRequest(service, authInfo, path, NULL, "users")
    },

    listApplicationGroups = function(applicationId) {
      path <- paste("/applications/", applicationId, "/authorization/groups",
                    sep="")
      listRequest(service, authInfo, path, NULL, "groups")
    },

    listApplicationInvitations = function(applicationId) {
      path <- "/invitations/"
      query <- paste(filterQuery("app_id", applicationId), collapse="&")
      listRequest(service, authInfo, path, query, "invitations")
    },

    listTasks = function(accountId, filters = NULL) {
      if (is.null(filters)) {
        filters <- vector()
      }
      path <- "/tasks/"
      filters <- c(filterQuery("account_id", accountId), filters)
      query <- paste(filters, collapse="&")
      listRequest(service, authInfo, path, query, "tasks", max=100)
    },

    getTaskInfo = function(taskId) {
      path <- paste("/tasks/", taskId, sep="")
      handleResponse(GET(service, authInfo, path))
    },

    getTaskLogs = function(taskId) {
      path <- paste("/tasks/", taskId, "/logs/", sep="")
      handleResponse(GET(service, authInfo, path))
    },

    waitForTask = function(taskId, quiet = FALSE) {

      if (!quiet) {
        cat("Waiting for task: ", taskId, "\n", sep="")
      }

      path <- paste("/tasks/", taskId, sep="")

      lastStatus <- NULL
      while(TRUE) {

        # check status
        status <- handleResponse(GET(service, authInfo, path))

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
            hr("Begin Task Log")
            taskLog(taskId, authInfo$name, authInfo$server, output="stderr")
            hr("End Task Log")
            stop(status$error, call. = FALSE)
          }
        }

        # wait for 1 second before polling again
        Sys.sleep(1)
      }
    }
  )
}

listRequest = function(service, authInfo, path, query, listName, page = 100,
                       max=NULL) {

  # accumulate multiple pages of results
  offset <- 0
  results <- list()

  while(TRUE) {

    # add query params
    queryWithList <- paste(query, "&count=", page, "&offset=", offset, sep="")

    # make request and append the results
    response <- handleResponse(GET(service, authInfo, path, queryWithList))
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
