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
      GET(service, authInfo, unversioned_url("server_settings"))
    },

    ## User API

    currentUser = function() {
      # All callers only need $id and $username,
      # passed to registerAccount() (where account means user)
      # and that gets written to a .dcf file
      # /v1/user/ does not include $id
      # But it looks like none of the Connect code paths use the account/user id,
      # username is used to identify the "account", so this should be safe
      # to upgrade to v1.
      GET(service, authInfo, unversioned_url("users", "current"))
    },

    ## Tokens API

    addToken = function(token) {
      POST_JSON(service, authInfo, unversioned_url("tokens"), token)
    },

    ## Applications API

    listApplications = function(accountId, filters = NULL) {
      if (is.null(filters)) {
        filters <- vector()
      }
      path <- unversioned_url("applications")
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

      # Connect doesn't use the template or account ID
      # parameters; they exist for compatibility with lucid.
      result <- POST_JSON(service, authInfo, v1_url("content"), details)
      list(
        id = result$id,
        guid = result$guid,
        url = result$content_url,
        # Include dashboard_url so we can open it or logs path after deploy
        dashboard_url = result$dashboard_url
      )
    },

    uploadBundle = function(contentGuid, bundlePath) {
      path <- v1_url("content", contentGuid, "bundles")
      POST(
        service,
        authInfo,
        path,
        contentType = "application/x-gzip",
        file = bundlePath
      )
    },

    deployApplication = function(application, bundleId = NULL) {
      path <- v1_url("content", application$guid, "deploy")
      POST_JSON(
        service,
        authInfo,
        path,
        json = list(bundle_id = bundleId)
      )
    },

    getApplication = function(applicationId, deploymentRecordVersion) {
      GET(service, authInfo, unversioned_url("applications", applicationId))
    },

    waitForTask = function(taskId, quiet = FALSE) {
      path <- v1_url("tasks", taskId)
      query <- list(first = 0, wait = 1)

      while (TRUE) {
        # ick, manual url construction
        queryString <- paste(names(query), query, sep = "=", collapse = "&")
        url <- paste0(path, "?", queryString)

        response <- GET(service, authInfo, url)

        if (length(response$output) > 0) {
          if (!quiet) {
            messages <- unlist(response$output)
            messages <- stripConnectTimestamps(messages)

            # Made headers more prominent.
            heading <- grepl("^# ", messages)
            messages[heading] <- cli::style_bold(messages[heading])
            cat(paste0(messages, "\n", collapse = ""))
          }

          query$first <- response$last
        }

        if (length(response$finished) > 0 && response$finished) {
          return(response)
        }
      }
    },

    # - Environment variables -----------------------------------------------
    # https://docs.posit.co/connect/api/#get-/v1/content/{guid}/environment

    getEnvVars = function(guid) {
      path <- v1_url("content", guid, "environment")
      as.character(unlist(GET(service, authInfo, path, list())))
    },

    setEnvVars = function(guid, vars) {
      path <- v1_url("content", guid, "environment")
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

# Utilities for URL construction
# Also to make it easier to identify where we're calling public APIs and not
v1_url <- function(...) {
  # Start with empty string so we get a leading slash
  paste("", "v1", ..., sep = "/")
}

unversioned_url <- function(...) {
  paste("", ..., sep = "/")
}
