userRecord <- function(email, username, first_name, last_name, password) {
  list(
    email = email,
    username = username,
    first_name = first_name,
    last_name = last_name,
    password = password
  )
}

prettyPasteFields <- function(message, fields) {
  header <- paste(message, ":\n- ", sep = "")
  body <- paste(strwrap(paste(shQuote(fields), collapse = ", ")),
                collapse = "\n")
  paste(header, body, sep = "")
}

validateUserRecord <- function(record) {
  requiredFields <- c("email", "username", "first_name", "last_name", "password")
  missingFields <- setdiff(requiredFields, names(record))
  extraFields <- setdiff(names(record), requiredFields)

  ## Construct error message if necessary
  msg <- NULL
  if (length(missingFields)) {
    msg <- prettyPasteFields("The following required fields are missing",
                             missingFields)
  }
  if (length(extraFields)) {
    msg <- paste(msg, prettyPasteFields("The following extraneous fields were found",
                                        extraFields))
  }

  if (!is.null(msg)) {
    stop(msg)
  }
  record
}

# return a list of functions that can be used to interact with connect
connectClient <- function(service, authInfo) {
  service <- parseHttpUrl(service)

  list(

    ## User API

    addUser = function(userRecord) {
      userRecord <- validateUserRecord(userRecord)
      handleResponse(POST_JSON(service,
                               authInfo,
                               "/users",
                               userRecord))
    },

    getUser = function(userId) {
      handleResponse(GET(service, authInfo,
                         file.path("/users", userId)))
    },

    currentUser = function() {
      handleResponse(GET(service, authInfo, "/users/current/"))
    },

    ## Tokens API

    addToken = function(token) {
      handleResponse(POST_JSON(service,
                               authInfo,
                               "/tokens",
                               token))
    },

    getToken = function(tokenId) {
      handleResponse(GET(service, authInfo,
                         file.path("/tokens", tokenId)))
    },

    ## Applications API

    listApplications = function(accountId, filters = NULL) {
      if (is.null(filters)) {
        filters <- vector()
      }
      path <- "/applications/"
      query <- paste(filterQuery(
        c("account_id", names(filters)),
        c(accountId, unname(filters))
      ), collapse = "&")
      listRequest(service, authInfo, path, query, "applications")
    },

    createApplication = function(name, template, accountId) {
      # RSC doesn't currently use the template or account ID
      # parameters; they exist for compatibility with lucid.
      handleResponse(POST_JSON(service,
                               authInfo,
                               "/applications",
                               list(name = name)))
    },

    terminateApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, "/terminate", sep="")
      handleResponse(POST_JSON(service, authInfo, path, list()))
    },

    uploadApplication = function(appId, bundlePath) {
      path <- file.path("/applications", appId, "upload")
      handleResponse(POST(service, authInfo, path,
                          contentType="application/x-gzip",
                          file=bundlePath))
    },

    deployApplication = function(applicationId, bundleId=NULL) {
      path <- paste("/applications/", applicationId, "/deploy", sep="")
      json <- list()
      json$bundle <- as.numeric(bundleId)
      handleResponse(POST_JSON(service, authInfo, path, json))
    },

    configureApplication = function(applicationId) {
      handleResponse(GET(service, authInfo, paste(
        "/applications/", applicationId, "/config", sep="")))
    },

    getApplication = function(applicationId) {
      handleResponse(GET(service, authInfo, paste0("/applications/",
                                                   applicationId)))
    },

    ## Tasks API

    listTasks = function() {
      path <- "/tasks"
      handleResponse(GET(service,
                         authInfo,
                         path))
    },

    getTask = function(taskId) {
      path <- file.path("/tasks", taskId)
      handleResponse(GET(service,
                         authInfo,
                         path))
    },

    killTask = function(taskId) {
      path <- file.path("/tasks", taskId, "kill")
      handleResponse(POST_JSON(service,
                               authInfo,
                               path,
                               list()))
    },

    waitForTask = function(taskId, quiet) {
      start <- 0
      while (TRUE) {
        path <- paste0(file.path("/tasks", taskId), "?first_status=", start)
        response <- handleResponse(GET(service, authInfo, path))
        if (length(response$status) > 0) {
          lapply(response$status, message)
          start <- response$last_status
        }
        if (response$finished) {
          return(response)
        }
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
    pathWithQuery <- paste(path, "?", query,
                           "&count=", page,
                           "&offset=", offset,
                           sep="")

    # make request and append the results
    response <- handleResponse(GET(service, authInfo, pathWithQuery))
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
