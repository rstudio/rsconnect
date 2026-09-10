# Docs: https://posit-hosted.github.io/vivid-api

# Resolves the browsable URL for `contentId`, based on the account it
# actually belongs to (`accountId`) rather than the caller's own account --
# necessary because content may belong to a different (e.g. team) account
# than the one authenticating the request. `getAccounts` is a zero-arg
# function returning the accounts the caller has a role on (shared by
# `connectCloudClient()$getAccounts` and `migrateToConnectCloud()`).
connectCloudContentUrl <- function(getAccounts, accountId, contentId) {
  ownerAccount <- Find(
    function(a) identical(a$id, accountId),
    getAccounts()$data
  )
  if (is.null(ownerAccount)) {
    cli::cli_abort(
      c(
        "Unable to determine the Connect Cloud account for content {.val {contentId}}.",
        i = "You may not have access to the account this content belongs to."
      )
    )
  }
  paste0(connectCloudUrls()$ui, "/", ownerAccount$name, "/content/", contentId)
}

# Standalone (served) content URL -- the link handed to app consumers, and the
# value returned in the `url` column of `applications()`. Consistent with the
# served URL that shinyapps.io and Posit Connect report. The canonical scheme is
# <content-id>.share.<connect-cloud-host> (e.g.
# https://abc-123.share.connect.posit.cloud/). Derived from the UI base so it
# tracks the active environment (production/staging/development).
connectCloudStandaloneUrl <- function(contentId) {
  host <- sub("^https?://", "", connectCloudUrls()$ui)
  paste0("https://", contentId, ".share.", host, "/")
}

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

  getContent <- function(contentId) {
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
  }

  # Accumulate every page of a paginated GET list endpoint. `buildPath(limit,
  # offset)` returns the request path for a single page; it must include
  # `include_total=true` and a stable `order_by` so offset paging can't skip or
  # duplicate rows. Callers do any post-filtering on the returned list.
  paginate <- function(buildPath, pageSize = 100) {
    offset <- 0
    allItems <- list()
    repeat {
      response <- withTokenRefreshRetry(GET, buildPath(pageSize, offset))
      allItems <- c(allItems, response$data)
      offset <- offset + length(response$data)
      total <- as.numeric(response$total)
      if (
        length(response$data) == 0L ||
          isTRUE(offset >= total) ||
          (length(total) == 0L && length(response$data) < pageSize)
      ) {
        break
      }
    }
    allItems
  }

  # Accumulates every account the caller has a role on (not just the first
  # page), since the content being migrated/published may belong to any of them.
  getAccounts <- function() {
    accounts <- paginate(function(limit, offset) {
      paste0(
        "/accounts?has_user_role=true&include_total=true&limit=",
        limit,
        "&offset=",
        offset
      )
    })
    list(data = accounts)
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
      # order_by gives the list a stable total order; without it offset-based
      # paging can skip or duplicate rows across requests.
      allItems <- paginate(function(limit, offset) {
        paste0(
          "/contents?account_id=",
          accountId,
          "&order_by=created_time&include_total=true&limit=",
          limit,
          "&offset=",
          offset
        )
      })
      # Drop content that has been soft-deleted but not yet hard-deleted: an
      # account owner can still see it in the window before the cleanup job runs.
      allItems <- Filter(
        function(item) !identical(item$state, "deleted"),
        allItems
      )
      # Set name = title so resolveApplication (which matches on app$name) works for PCC.
      items <- lapply(allItems, function(item) {
        item$name <- item$title
        item
      })
      # Honor filters$name with exact-match semantics to match the shinyapps.io
      # client contract (listApplications callers may pass filters$name).
      # NOTE: getAppByName()/getLogs() historically reached this block; getLogs()
      # is now guarded with checkShinyappsServer() so PCC callers never reach it.
      # The block is kept for listApplications contract parity with shinyapps.io:
      # removing it would silently break any future caller that passes filters$name.
      if (!is.null(filters$name)) {
        items <- Filter(
          function(item) identical(item$name, filters$name),
          items
        )
      }
      items
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

    getContent = getContent,

    getApplication = function(applicationId, deploymentRecordVersion) {
      content <- getContent(applicationId)
      content$name <- content$title
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

        if (!is.null(revision$publish_result)) {
          # Resolve the URL from the content's actual owning account, not the
          # locally authenticated one -- the content may belong to a
          # different (e.g. team) account than the one publishing it. Don't
          # let a failure here mask the actual publish result: fall back to
          # an empty URL and warn instead of aborting the whole deploy.
          # Content genuinely deleted right after publishing gets its own
          # message since we know exactly what happened; anything else
          # (transient API errors, pagination limits, etc.) gets a generic
          # one.
          contentUrl <- tryCatch(
            {
              content <- getContent(revision$content_id)
              connectCloudContentUrl(
                getAccounts,
                content$account_id,
                revision$content_id
              )
            },
            rsconnect_http_404 = function(e) {
              cli::cli_alert_warning(
                "The published content could not be found immediately after publishing; no URL is available."
              )
              ""
            },
            error = function(e) {
              cli::cli_alert_warning(
                "Failed to resolve the content URL: {e$message}"
              )
              ""
            }
          )

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

    getAccounts = getAccounts,

    listApplicationAuthorization = function(appId) {
      # order_by keeps offset-based paging stable (see paginate()).
      paginate(function(limit, offset) {
        paste0(
          "/contents/",
          appId,
          "/users?order_by=created_time&include_total=true&limit=",
          limit,
          "&offset=",
          offset
        )
      })
    },

    removeApplicationUser = function(appId, userId) {
      path <- paste0("/contents/", appId, "/users/", userId)
      withTokenRefreshRetry(DELETE, path)
      invisible(TRUE)
    },

    inviteApplicationUser = function(appId, email, sendEmail, emailMessage) {
      path <- paste0("/contents/", appId, "/invitations")
      json <- list(
        message = emailMessage,
        email_invitations = list(list(email_address = email)),
        recipient_invitations = list()
      )
      withTokenRefreshRetry(POST_JSON, path, json)
      invisible(TRUE)
    },

    listApplicationInvitations = function(appId) {
      # order_by keeps offset-based paging stable (see paginate()).
      paginate(function(limit, offset) {
        paste0(
          "/contents/",
          appId,
          "/invitations?accepted_time__isnull=true&order_by=created_time&include_total=true&limit=",
          limit,
          "&offset=",
          offset
        )
      })
    },

    resendApplicationInvitation = function(inviteId, regenerate) {
      # regenerate is shinyapps.io-specific; PCC re-sends the existing invite email.
      # Use setNames(list(), character(0)) to produce {} not [] at the wire level.
      path <- paste0("/content_invitations/", inviteId, "/resend")
      withTokenRefreshRetry(POST_JSON, path, setNames(list(), character(0)))
      invisible(TRUE)
    }
  )
}
