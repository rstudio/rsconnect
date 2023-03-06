connectClient <- function(service, authInfo) {
  list(

    ## Server settings API

    serverSettings = function() {
      handleResponse(GET(service, authInfo, file.path("/server_settings")))
    },

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
      handleResponse(GET(service, authInfo, "/users/current"))
    },

    ## Tokens API

    addToken = function(token) {
      handleResponse(POST_JSON(service,
                               authInfo,
                               "/tokens",
                               token))
    },

    ## Applications API

    listApplications = function(accountId, filters = NULL) {
      if (is.null(filters)) {
        filters <- vector()
      }
      path <- "/applications"
      query <- paste(filterQuery(
        c("account_id", names(filters)),
        c(accountId, unname(filters))
      ), collapse = "&")
      listRequest(service, authInfo, path, query, "applications")
    },

    createApplication = function(name, title, template, accountId) {
      # add name; inject title if specified
      details <- list(name = name)
      if (!is.null(title) && nzchar(title))
        details$title <- title

      # RSC doesn't currently use the template or account ID
      # parameters; they exist for compatibility with lucid.
      handleResponse(POST_JSON(service,
                               authInfo,
                               "/applications",
                               details))
    },

    terminateApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, "/terminate", sep = "")
      handleResponse(POST_JSON(service, authInfo, path, list()))
    },

    uploadApplication = function(appId, bundlePath) {
      path <- file.path("/applications", appId, "upload")
      handleResponse(POST(service, authInfo, path,
                          contentType = "application/x-gzip",
                          file = bundlePath))
    },

    deployApplication = function(applicationId, bundleId = NULL) {
      path <- paste("/applications/", applicationId, "/deploy", sep = "")
      json <- list()
      json$bundle <- as.numeric(bundleId)
      handleResponse(POST_JSON(service, authInfo, path, json))
    },

    configureApplication = function(applicationId) {
      handleResponse(GET(service, authInfo, paste(
        "/applications/", applicationId, "/config", sep = "")))
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
          messages <- unlist(response$status)

          # Strip timestamps, if found
          timestamp_re <- "\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{3,} "
          messages <- gsub(timestamp_re, "", messages)

          # Made headers more prominent.
          heading <- grepl("^# ", messages)
          messages[heading] <- cli::style_bold(messages[heading])
          cat(paste0(messages, "\n", collapse = ""))

          start <- response$last_status
        }

        if (length(response$finished) > 0 && response$finished) {
          return(response)
        }
        Sys.sleep(1)
      }
    }

  )

}

# userRecord --------------------------------------------------------------

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
