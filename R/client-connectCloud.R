# Docs: https://posit-hosted.github.io/vivid-api

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

  self <- structure(
    list(withTokenRefreshRetry = withTokenRefreshRetry),
    class = c("connectCloudClient", "rsconnectClient")
  )
  # The RStudio IDE calls client$getContent()
  self$getContent <- function(contentId) connectCloudGetContent(self, contentId)
  self
}

#' @export
uploadBundle.connectCloudClient <- function(client, application, bundlePath) {
  uploadUrl <- application$next_revision$source_bundle_upload_url
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

  if (response$status > 299) {
    cli::cli_abort("Could not upload bundle.")
  }
  # Connect Cloud has no bundle id, so the deploy template gets NULL here.
  NULL
}

#' @export
listApplications.connectCloudClient <- function(
  client,
  accountId,
  filters = list()
) {
  # order_by gives the list a stable total order; without it offset-based
  # paging can skip or duplicate rows across requests.
  allItems <- connectCloudPaginate(client, function(limit, offset) {
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
  # Each item gets a `name`, as on the other clients. Connect Cloud content has
  # only a title.
  items <- lapply(allItems, function(item) {
    item$name <- item$title
    item
  })
  # Match filters$name exactly, like the other clients do.
  if (!is.null(filters$name)) {
    items <- Filter(
      function(item) identical(item$name, filters$name),
      items
    )
  }
  items
}

#' @export
getApplication.connectCloudClient <- function(client, applicationId) {
  content <- connectCloudGetContent(client, applicationId)
  content$name <- generateAppName(content$title, unique = FALSE)
  content
}

# Connect Cloud content has no `url` until it is published.
#' @export
createContent.connectCloudClient <- function(
  client,
  deployment,
  accountDetails,
  appMetadata,
  appVisibility = NULL
) {
  title <- if (nzchar(deployment$title)) deployment$title else deployment$name
  revision <- list(
    source_type = "bundle",
    content_type = cloudContentTypeFromAppMode(appMetadata$appMode),
    app_mode = appMetadata$appMode,
    primary_file = connectCloudPrimaryFile(appMetadata)
  )

  json <- list(
    account_id = accountDetails$accountId,
    title = title,
    next_revision = revision,
    secrets = cloudSecrets(deployment$envVars)
  )
  # Omit when NULL so the server picks the default for the account's plan.
  json$access <- appVisibility

  content <- client$withTokenRefreshRetry(POST_JSON, "/contents", json)
  content$application_id <- content$id
  content
}

#' @export
findContent.connectCloudClient <- function(client, deployment, quiet) {
  application <- connectCloudGetContent(client, deployment$appId)
  taskComplete(quiet, "Found content")
  application
}

#' @export
prepareContent.connectCloudClient <- function(
  client,
  application,
  deployment,
  appMetadata,
  appVisibility,
  isNewContent,
  upload,
  quiet
) {
  # New content already has a fresh pending revision from createContent().
  # Existing content needs a new revision and upload URL.
  if (isNewContent) {
    return(application)
  }
  taskStart(quiet, "Updating content...")
  path <- paste0("/contents/", application$id)
  if (upload) {
    path <- paste0(path, "?new_bundle=true")
  }
  json <- list(
    secrets = cloudSecrets(deployment$envVars),
    revision_overrides = list(
      primary_file = connectCloudPrimaryFile(appMetadata),
      app_mode = appMetadata$appMode
    )
  )
  # Omit when NULL so a redeploy keeps visibility changed in the Cloud UI.
  json$access <- appVisibility

  content <- client$withTokenRefreshRetry(PATCH_JSON, path, json)
  content$application_id <- content$id
  taskComplete(quiet, "Content updated")
  content
}

#' @export
activateContent.connectCloudClient <- function(
  client,
  application,
  bundle,
  quiet
) {
  path <- paste0("/contents/", application$id, "/publish")
  client$withTokenRefreshRetry(POST_JSON, path, list())
  response <- awaitConnectCloudCompletion(client, application$next_revision$id)
  list(
    succeeded = response$success,
    url = response$url,
    error = response$error
  )
}

#' @export
serverDisplayName.connectCloudClient <- function(client) {
  "Posit Connect Cloud"
}

#' @export
supportsEnvVars.connectCloudClient <- function(client) {
  TRUE
}

#' @export
supportsEnvVarManagement.connectCloudClient <- function(client) {
  FALSE
}

#' @export
supportsNodejs.connectCloudClient <- function(client) {
  FALSE
}

#' @export
usesPasswordFile.connectCloudClient <- function(client) {
  FALSE
}

# Connect Cloud always sends the invitation email.
#' @export
supportsOptionalInviteEmail.connectCloudClient <- function(client) {
  FALSE
}

# Connect Cloud can return a user record with a redacted email.
#' @export
redactsUserEmails.connectCloudClient <- function(client) {
  TRUE
}

#' @export
requiresUpload.connectCloudClient <- function(client) {
  FALSE
}

#' @export
pythonEnabledByDefault.connectCloudClient <- function(client) {
  TRUE
}

#' @export
validateVisibility.connectCloudClient <- function(
  client,
  appVisibility,
  error_call
) {
  values <- c(
    "private",
    "public",
    "view_team_edit_private",
    "view_team_edit_team",
    "view_public_edit_team"
  )
  arg_match(appVisibility, values, error_call = error_call)
  invisible()
}

#' @export
supportsMetadataSync.connectCloudClient <- function(client) {
  FALSE
}

#' @export
staticRmdNeedsShiny.connectCloudClient <- function(client) {
  FALSE
}

#' @export
addsUtmParameters.connectCloudClient <- function(client) {
  TRUE
}

#' @export
currentUser.connectCloudClient <- function(client) {
  client$withTokenRefreshRetry(GET, "/users/me")
}

# Connect Cloud titles can change and do not have to be unique, so a deploy
# without a deployment record always makes new content.
#' @export
findContentByName.connectCloudClient <- function(client, accountDetails, name) {
  NULL
}

# A content id targets the content directly. If there is no content id, the id
# comes from the local deployment record. The title cannot identify the
# content, because Connect Cloud titles can change and do not have to be unique.
#' @export
resolveContentTarget.connectCloudClient <- function(
  client,
  accountDetails,
  appDir,
  appName,
  contentId = NULL
) {
  if (!is.null(contentId)) {
    check_string(contentId)
    return(list(id = contentId, deploymentFile = NULL))
  }
  recs <- deployments(
    appPath = appDir,
    accountFilter = accountDetails$name,
    serverFilter = accountDetails$server,
    nameFilter = appName
  )
  if (nrow(recs) == 0L) {
    cli::cli_abort(c(
      "Can't identify the Posit Connect Cloud content for {.file {appDir}}.",
      i = paste0(
        "No deployment record found. Deploy the content first, or run from ",
        "the project directory that contains its {.path rsconnect/} deployment record."
      )
    ))
  }
  if (nrow(recs) > 1L) {
    dep <- disambiguateDeployments(recs)
    return(list(id = dep$appId, deploymentFile = dep$deploymentFile))
  }
  list(id = recs$appId[[1L]], deploymentFile = recs$deploymentFile[[1L]])
}

# The `account` column is always `NA`, because Connect Cloud users do not have
# an account name.
#' @export
listCollaborators.connectCloudClient <- function(client, applicationId) {
  res <- connectCloudListApplicationAuthorization(client, applicationId)
  rows <- lapply(res, function(x) {
    id <- as.character(x$user$id %||% NA_character_)
    email <- as.character(x$user$email %||% NA_character_)
    checkCollaboratorRecord(client, id, email)
    data.frame(
      id = id,
      email = email,
      account = NA_character_,
      display_name = as.character(x$user$display_name %||% NA_character_),
      role = as.character(x$role %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    return(data.frame(
      id = character(),
      email = character(),
      account = character(),
      display_name = character(),
      role = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' @export
inviteApplicationUser.connectCloudClient <- function(
  client,
  applicationId,
  email,
  sendEmail = NULL,
  emailMessage = NULL
) {
  path <- paste0("/contents/", applicationId, "/invitations")
  json <- list(
    message = emailMessage,
    email_invitations = list(list(email_address = email)),
    recipient_invitations = list()
  )
  client$withTokenRefreshRetry(POST_JSON, path, json)
  invisible(TRUE)
}

#' @export
removeApplicationUser.connectCloudClient <- function(
  client,
  applicationId,
  userId
) {
  path <- paste0("/contents/", applicationId, "/users/", userId)
  client$withTokenRefreshRetry(DELETE, path)
  invisible(TRUE)
}

# Connect Cloud sends the same invitation email again, so it ignores
# `regenerate`. setNames(list(), character(0)) sends `{}` and not `[]`.
#' @export
resendApplicationInvitation.connectCloudClient <- function(
  client,
  invitationId,
  regenerate = FALSE
) {
  path <- paste0("/content_invitations/", invitationId, "/resend")
  client$withTokenRefreshRetry(POST_JSON, path, setNames(list(), character(0)))
  invisible(TRUE)
}

#' @export
listInvitations.connectCloudClient <- function(client, applicationId) {
  res <- connectCloudListApplicationInvitations(client, applicationId)
  rows <- lapply(res, function(x) {
    data.frame(
      id = as.character(x$id %||% NA_character_),
      email = as.character(x$email_address %||% NA_character_),
      link = as.character(x$link %||% NA_character_),
      expired = as.logical(x$is_expired %||% NA),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    return(emptyInvitations())
  }
  do.call(rbind, rows)
}

#' @export
applicationsTable.connectCloudClient <- function(client, accountDetails) {
  apps <- listApplications(client, accountDetails$accountId)
  empty <- data.frame(
    id = character(),
    name = character(),
    title = character(),
    url = character(),
    status = character(),
    size = character(),
    instances = integer(),
    config_url = character(),
    created_time = character(),
    updated_time = character(),
    guid = character(),
    stringsAsFactors = FALSE
  )
  if (length(apps) == 0) {
    return(empty)
  }
  # Resolve the owning account's real server-side slug once. All items belong
  # to accountDetails$accountId (listApplications filters by it), so one
  # connectCloudGetAccounts() call covers every row. Using the resolved slug
  # rather than accountDetails$name (the local alias) prevents wrong-account
  # URLs when the remote slug differs from the alias stored in the local config.
  pccAccts <- connectCloudGetAccounts(client)$data
  pccOwner <- Find(
    function(a) identical(a$id, accountDetails$accountId),
    pccAccts
  )
  if (is.null(pccOwner)) {
    cli::cli_abort(
      c(
        "Unable to determine the Connect Cloud account for content listing.",
        i = "You may not have access to the account this content belongs to."
      )
    )
  }
  contentUrlBase <- paste0(
    connectCloudUrls()$ui,
    "/",
    pccOwner$name,
    "/content/"
  )
  res <- lapply(apps, function(x) {
    # url = the standalone served URL handed to app consumers (consistent with
    # shinyapps.io/Connect); config_url = the dashboard settings page.
    # Prefer the revision's served URL (the vanity/custom URL when set); fall
    # back to the constructed content-id URL for content not yet published,
    # where current_revision (or its url) is absent.
    contentId <- x$id %||% ""
    dashboardUrl <- paste0(contentUrlBase, contentId)
    data.frame(
      id = x$id %||% NA_character_,
      name = x$title %||% NA_character_,
      title = x$title %||% NA_character_,
      url = x$current_revision$url %||% connectCloudStandaloneUrl(contentId),
      status = NA_character_,
      size = NA_character_,
      instances = NA_integer_,
      config_url = paste0(dashboardUrl, "/settings/info"),
      created_time = x$created_time %||% NA_character_,
      updated_time = x$updated_time %||% NA_character_,
      guid = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call("rbind", res)
}

connectCloudGetAuthorization <- function(client, logChannel) {
  json <- list(
    resource_type = "log_channel",
    resource_id = logChannel,
    permission = "revision.logs:read"
  )

  response <- client$withTokenRefreshRetry(
    POST_JSON,
    "/authorization",
    json
  )

  # Return the token from the response
  response$token
}

connectCloudGetContent <- function(client, contentId) {
  path <- paste0("/contents/", contentId)
  content <- client$withTokenRefreshRetry(GET, path)
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
connectCloudPaginate <- function(client, buildPath, pageSize = 100) {
  offset <- 0
  allItems <- list()
  repeat {
    response <- client$withTokenRefreshRetry(GET, buildPath(pageSize, offset))
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
connectCloudGetAccounts <- function(client) {
  accounts <- connectCloudPaginate(client, function(limit, offset) {
    paste0(
      "/accounts?has_user_role=true&include_total=true&limit=",
      limit,
      "&offset=",
      offset
    )
  })
  list(data = accounts)
}

connectCloudListApplicationAuthorization <- function(client, appId) {
  # order_by keeps offset-based paging stable.
  connectCloudPaginate(client, function(limit, offset) {
    paste0(
      "/contents/",
      appId,
      "/users?order_by=created_time&include_total=true&limit=",
      limit,
      "&offset=",
      offset
    )
  })
}

connectCloudDeleteContent <- function(client, contentId) {
  path <- paste0("/contents/", contentId)
  client$withTokenRefreshRetry(DELETE, path)
  invisible(TRUE)
}

connectCloudListApplicationInvitations <- function(client, appId) {
  # order_by keeps offset-based paging stable.
  connectCloudPaginate(client, function(limit, offset) {
    paste0(
      "/contents/",
      appId,
      "/invitations?accepted_time__isnull=true&order_by=created_time&include_total=true&limit=",
      limit,
      "&offset=",
      offset
    )
  })
}

# Resolves the browsable URL for `contentId`, based on the account it
# actually belongs to (`accountId`) rather than the caller's own account --
# necessary because content may belong to a different (e.g. team) account
# than the one authenticating the request.
connectCloudContentUrl <- function(client, accountId, contentId) {
  ownerAccount <- Find(
    function(a) identical(a$id, accountId),
    connectCloudGetAccounts(client)$data
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

cloudSecrets <- function(envVars) {
  if (length(envVars) == 0L) {
    return(I(list()))
  }
  values <- Sys.getenv(envVars, unset = NA)
  keep <- !is.na(values)
  if (!any(keep)) {
    return(I(list()))
  }

  unname(Map(
    function(name, value) {
      list(
        name = name,
        value = value
      )
    },
    envVars[keep],
    values[keep]
  ))
}

# The primary file of the revision. Use appPrimaryDoc if it is set, otherwise
# use the inferred primary file.
connectCloudPrimaryFile <- function(appMetadata) {
  appMetadata$appPrimaryDoc %||% appMetadata$inferredPrimaryFile
}

# Polls the revision until the publish process completes, returning whether
# the publish request succeeded and the error message if it failed.
awaitConnectCloudCompletion <- function(client, revisionId) {
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
    revision <- client$withTokenRefreshRetry(GET, path)
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
          content <- connectCloudGetContent(client, revision$content_id)
          connectCloudContentUrl(
            client,
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
              authToken <- connectCloudGetAuthorization(
                client,
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
}
