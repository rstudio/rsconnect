# Docs: https://docs.posit.co/connect/api/

stripConnectTimestamps <- function(messages) {
  # Strip timestamps, if found
  timestamp_re <- "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{3,} "
  gsub(timestamp_re, "", messages)
}

connectClient <- function(service, authInfo) {
  list(

    service = function() {
      "connect"
    },

    ## Server settings API

    serverSettings = function() {
      GET(service, authInfo, file.path("/server_settings"))
    },

    ## User API

    addUser = function(userRecord) {
      userRecord <- validateUserRecord(userRecord)
      POST_JSON(service, authInfo, "/users", userRecord)
    },

    getUser = function(userId) {
      GET(service, authInfo, file.path("/users", userId))
    },

    currentUser = function() {
      GET(service, authInfo, "/users/current")
    },

    ## Tokens API

    addToken = function(token) {
      POST_JSON(service, authInfo, "/tokens", token)
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
      listApplicationsRequest(service, authInfo, path, query, "applications")
    },

    createApplication = function(name, title, template, accountId, appMode, contentCategory = NULL, spaceId = NULL) {
      # add name; inject title if specified
      details <- list(name = name)
      if (!is.null(title) && nzchar(title))
        details$title <- title

      # RSC doesn't currently use the template or account ID
      # parameters; they exist for compatibility with lucid.
      application <- POST_JSON(service, authInfo, "/applications", details)
      list(
        id = application$id,
        guid = application$guid,
        url = application$url
      )
    },

    terminateApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, "/terminate", sep = "")
      POST_JSON(service, authInfo, path, list())
    },

    uploadApplication = function(appId, bundlePath) {
      path <- file.path("/applications", appId, "upload")
      POST(
        service,
        authInfo,
        path,
        contentType = "application/x-gzip",
        file = bundlePath
      )
    },

    deployApplication = function(application, bundleId = NULL, spaceId = NULL) {
      path <- paste("/applications/", application$id, "/deploy", sep = "")
      json <- list()
      json$bundle <- as.numeric(bundleId)
      POST_JSON(service, authInfo, path, json)
    },

    configureApplication = function(applicationId) {
      GET(service, authInfo, paste(
        "/applications/", applicationId, "/config", sep = ""))
    },

    getApplication = function(applicationId, deploymentRecordVersion) {
      GET(service, authInfo, paste0("/applications/", applicationId))
    },

    waitForTask = function(taskId, quiet = FALSE) {
      first <- 0
      wait <- 1
      while (TRUE) {
        path <- paste0(
          "/v1/tasks/", taskId,
          "?first=", first,
          "&wait=", wait)
        response <- GET(service, authInfo, path)

        if (length(response$output) > 0) {
          if (!quiet) {
            messages <- unlist(response$output)
            messages <- stripConnectTimestamps(messages)

            # Made headers more prominent.
            heading <- grepl("^# ", messages)
            messages[heading] <- cli::style_bold(messages[heading])
            cat(paste0(messages, "\n", collapse = ""))
          }

          first <- response$last
        }

        if (length(response$finished) > 0 && response$finished) {
          return(response)
        }
      }
    },

    # - Environment variables -----------------------------------------------
    # https://docs.posit.co/connect/api/#get-/v1/content/{guid}/environment

    getEnvVars = function(guid) {
      path <- file.path("/v1/content", guid, "environment")
      as.character(unlist(GET(service, authInfo, path, list())))
    },

    setEnvVars = function(guid, vars) {
      path <- file.path("/v1/content", guid, "environment")
      body <- unname(Map(
        function(name, value) {
          list(
            name = name,
            value = if (is.na(value)) NULL else value
          )
        },
        vars,
        Sys.getenv(vars, unset = NA)
      ))
      PATCH_JSON(service, authInfo, path, body)
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
