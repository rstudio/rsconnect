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
      query <- paste(
        filterQuery(
          c("account_id", names(filters)),
          c(accountId, unname(filters))
        ),
        collapse = "&"
      )
      listApplicationsRequest(service, authInfo, path, query, "applications")
    },

    createApplication = function(
      name,
      title,
      template,
      accountId,
      appMode,
      contentCategory = NULL
    ) {
      # add name; inject title if specified
      details <- list(name = name)
      if (!is.null(title) && nzchar(title)) {
        details$title <- title
      }

      # RSC doesn't currently use the template or account ID
      # parameters; they exist for compatibility with lucid.
      application <- POST_JSON(service, authInfo, "/applications", details)
      list(
        id = application$id,
        guid = application$guid,
        url = application$url
      )
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

    deployApplication = function(application, bundleId = NULL) {
      path <- paste("/applications/", application$id, "/deploy", sep = "")
      json <- list()
      json$bundle <- as.numeric(bundleId)
      POST_JSON(service, authInfo, path, json)
    },

    configureApplication = function(applicationId) {
      GET(
        service,
        authInfo,
        paste(
          "/applications/",
          applicationId,
          "/config",
          sep = ""
        )
      )
    },

    getApplication = function(applicationId, deploymentRecordVersion) {
      GET(service, authInfo, paste0("/applications/", applicationId))
    },

    waitForTask = function(taskId, quiet = FALSE) {
      first <- 0
      wait <- 1
      while (TRUE) {
        path <- paste0(
          "/v1/tasks/",
          taskId,
          "?first=",
          first,
          "&wait=",
          wait
        )
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

getSnowflakeAuthToken <- function(url, snowflakeConnectionName) {
  parsedURL <- parseHttpUrl(url)
  ingressURL <- parsedURL$host

  token <- snowflakeauth::snowflake_credentials(
    snowflakeauth::snowflake_connection(snowflakeConnectionName),
    spcs_endpoint = ingressURL
  )

  token
}
