# Docs: https://posit-hosted.github.io/vivid-api

# Map rsconnect appMode to Connect Cloud contentType
cloudContentTypeFromAppMode <- function(appMode) {
  switch(
    appMode,
    "jupyter-notebook" = "jupyter",
    "python-bokeh" = "bokeh",
    "python-dash" = "dash",
    "python-shiny" = "shiny",
    "shiny" = "shiny",
    "python-streamlit" = "streamlit",
    "quarto" = "quarto",
    "quarto-static" = "quarto",
    "quarto-shiny" = "quarto",
    "rmd-static" = "rmarkdown",
    "rmd-shiny" = "rmarkdown",
    "static" = "static",
    stop(
      "appMode '",
      appMode,
      "' is not supported by Connect Cloud",
      call. = FALSE
    )
  )
}

# Creates a client for interacting with the Connect Cloud API.
connectCloudClient <- function(service, authInfo) {
  # Generic retry wrapper. If a request fails with 401 Unauthorized, it will
  # mint a new access token (via client_credentials when the account was
  # registered with a clientSecret, otherwise via refresh_token) and retry
  # the request once.
  withTokenRefreshRetry <- function(request_fn, ...) {
    tryCatch(
      {
        request_fn(service, authInfo, ...)
      },
      rsconnect_http_401 = function(e) {
        authClient <- cloudAuthClient()
        if (!is.null(authInfo$clientSecret)) {
          # Prefer client_credentials when the account has it: the secret is
          # long-lived and caller-controlled, while any refresh_token returned
          # alongside a client_credentials grant is non-standard
          # (RFC 6749 §4.4.3) and shouldn't be relied on.
          tokenResponse <- authClient$exchangeClientCredentials(
            authInfo$clientId,
            authInfo$clientSecret
          )
        } else {
          tokenResponse <- authClient$exchangeToken(list(
            grant_type = "refresh_token",
            refresh_token = authInfo$refreshToken
          ))
        }

        # registerAccount writes a fresh DCF from the fields passed (no merge),
        # so pass clientId/clientSecret through to keep them on disk for the
        # next deploy.
        registerAccount(
          authInfo$server,
          authInfo$name,
          authInfo$accountId,
          accessToken = tokenResponse$access_token,
          refreshToken = tokenResponse$refresh_token,
          clientId = authInfo$clientId,
          clientSecret = authInfo$clientSecret
        )

        # Retry the original request with refreshed token
        authInfo$accessToken <<- tokenResponse$access_token
        authInfo$refreshToken <<- tokenResponse$refresh_token
        request_fn(service, authInfo, ...)
      }
    )
  }

  getAuthorization <- function(logChannel) {
    json <- list(
      resource_type = "log_channel",
      resource_id = logChannel,
      permission = "revision.logs:read"
    )

    response <- withTokenRefreshRetry(
      POST_JSON,
      "/authorization",
      json
    )

    # Return the token from the response
    response$token
  }

  list(
    service = function() {
      "connect.posit.cloud"
    },

    currentUser = function() {
      GET(service, authInfo, "/users/me")
    },

    withTokenRefreshRetry = withTokenRefreshRetry,

    listApplications = function(accountId, filters = list()) {
      # TODO: call the real API when available (api doesn't support filtering by name yet)
      return(list())
    },

    createContent = function(
      name,
      title,
      accountId,
      appMode,
      primaryFile,
      envVars
    ) {
      title <- if (nzchar(title)) title else name
      contentType <- cloudContentTypeFromAppMode(appMode)

      # Build revision object, conditionally including primary_file
      revision <- list(
        source_type = "bundle",
        content_type = contentType,
        app_mode = appMode,
        primary_file = primaryFile
      )

      secrets <- unname(Map(
        function(name, value) {
          list(
            name = name,
            value = value
          )
        },
        envVars,
        Sys.getenv(envVars)
      ))

      json <- list(
        account_id = accountId,
        title = title,
        next_revision = revision,
        secrets = secrets
      )

      content <- withTokenRefreshRetry(
        POST_JSON,
        "/contents",
        json
      )
      content$application_id <- content$id
      content
    },

    getContent = function(contentId) {
      path <- paste0("/contents/", contentId)
      content <- withTokenRefreshRetry(GET, path)
      if (content$state == "deleted") {
        cli::cli_abort(
          "Content is pending deletion.",
          class = c(
            "rsconnect_http_404",
            "rsconnect_http"
          )
        )
      }
      content
    },

    updateContent = function(
      contentId,
      envVars,
      newBundle = FALSE,
      primaryFile,
      appMode
    ) {
      path <- paste0("/contents/", contentId)
      if (newBundle) {
        path <- paste0(path, "?new_bundle=true")
      }

      secrets <- unname(Map(
        function(name, value) {
          list(
            name = name,
            value = value
          )
        },
        envVars,
        Sys.getenv(envVars)
      ))

      json <- list(
        secrets = secrets,
        revision_overrides = list(
          primary_file = primaryFile,
          app_mode = appMode
        )
      )

      content <- withTokenRefreshRetry(PATCH_JSON, path, json)
      content$application_id <- content$id
      content
    },

    uploadBundle = function(bundlePath, uploadUrl) {
      uploadService <- parseHttpUrl(uploadUrl)
      headers <- list()
      headers$`Content-Type` <- "application/gzip"

      response <- httpLibCurl(
        uploadService$protocol,
        uploadService$host,
        uploadService$port,
        "POST",
        uploadService$path,
        headers,
        headers$`Content-Type`,
        bundlePath
      )

      response$status <= 299
    },

    publish = function(contentId) {
      path <- paste0("/contents/", contentId, "/publish")
      withTokenRefreshRetry(POST_JSON, path, list())
    },

    # Polls the revision until the publish process completes, returning whether
    # the publish request succeeded and the error message if it failed.
    awaitCompletion = function(revisionId) {
      stateMessages <- list(
        publish_deferred = "Content is currently publishing; your request will start soon.",
        publish_requested = "Publish requested; waiting to start...",
        publish_started = "Publish started.",
        fetching = "Retrieving code...",
        building = "Installing dependencies...",
        rendering = "Rendering...",
        publishing = "Publishing content...",
        published = "Done."
      )
      lastStatus <- NULL
      repeat {
        path <- paste0("/revisions/", revisionId)
        revision <- withTokenRefreshRetry(GET, path)
        newStatus <- revision$status
        if (!isTRUE(newStatus == lastStatus)) {
          # Note: since we poll every second, it's possible to skip states in
          # the output here
          cli::cli_alert_info(stateMessages[[newStatus]])
          lastStatus <- newStatus
        }

        contentUrl <- paste0(
          connectCloudUrls()$ui,
          "/",
          authInfo$username,
          "/content/",
          revision$content_id
        )

        if (!is.null(revision$publish_result)) {
          if (revision$publish_result == "failure") {
            # Try to retrieve logs if log channel is available
            if (!is.null(revision$publish_log_channel)) {
              tryCatch(
                {
                  # Get authorization token for the log channel
                  authToken <- getAuthorization(
                    revision$publish_log_channel
                  )

                  # Create logs client and fetch logs
                  logsClient <- connectCloudLogsClient()
                  logs <- logsClient$getLogs(
                    revision$publish_log_channel,
                    authToken
                  )

                  # Print logs to stderr
                  if (!is.null(logs) && !is.null(logs$data)) {
                    cli::cat_rule(
                      "Begin Publishing Log",
                      line = "#",
                      file = stderr()
                    )
                    for (log_entry in logs$data) {
                      local_timestamp <- as.POSIXct(
                        # Convert to seconds
                        log_entry$timestamp / 1e6,
                        origin = "1970-01-01",
                      )
                      # Format with millisecond precision
                      formatted_timestamp <- format(
                        local_timestamp,
                        "%Y-%m-%d %H:%M:%OS3"
                      )
                      cat(
                        sprintf(
                          "[%s] %s: %s\n",
                          formatted_timestamp,
                          toupper(log_entry$level),
                          log_entry$message
                        ),
                        file = stderr()
                      )
                    }
                    cli::cat_rule(
                      "End Publishing Log",
                      line = "#",
                      file = stderr()
                    )
                  }
                },
                error = function(e) {
                  # If log retrieval fails, continue without logs
                  # Don't fail the entire operation just because logs couldn't be retrieved
                  cli::cli_alert_warning(
                    "Failed to retrieve logs: {e$message}"
                  )
                }
              )
            }

            return(list(
              success = FALSE,
              url = contentUrl,
              error = revision$publish_error_details
            ))
          }

          return(list(success = TRUE, url = contentUrl, error = NULL))
        }

        Sys.sleep(1)
      }
    },

    getAuthorization = getAuthorization,

    getAccounts = function(revisionId) {
      GET(service, authInfo, "/accounts?has_user_role=true")
    }
  )
}
