# Docs: https://docs.posit.co/connect/api/

connectClient <- function(service, authInfo) {
  self <- structure(
    list(
      # The connection identity. Client functions read these to make requests.
      service = service,
      authInfo = authInfo
    ),
    class = c("connectClient", "rsconnectClient")
  )
  # The RStudio IDE calls client$getEnvVars()
  self$getEnvVars <- function(guid) connectGetEnvVars(self, guid)
  self
}

#' @export
uploadBundle.connectClient <- function(client, application, bundlePath) {
  path <- v1_url("content", application$guid, "bundles")
  POST(
    client$service,
    client$authInfo,
    path,
    contentType = "application/x-gzip",
    file = bundlePath
  )
}

#' @export
listApplications.connectClient <- function(
  client,
  accountId,
  filters = list()
) {
  if (length(filters) == 0) {
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
  listApplicationsRequest(
    client$service,
    client$authInfo,
    path,
    query,
    "applications"
  )
}

# The deployment record has only the numeric id, and not the guid that
# v1/content/ needs, so this uses the unversioned URL.
#' @export
getApplication.connectClient <- function(client, applicationId) {
  app <- GET(
    client$service,
    client$authInfo,
    unversioned_url("applications", applicationId)
  )
  # Add dashboard_url, which comes in the v1/content URL but not applications/
  app$dashboard_url <- connectDashboardUrl(
    buildHttpUrl(client$service),
    app$guid
  )
  app
}

#' @export
createContent.connectClient <- function(
  client,
  deployment,
  accountDetails,
  appMetadata,
  appVisibility = NULL
) {
  details <- list(name = deployment$name)
  if (!is.null(deployment$title) && nzchar(deployment$title)) {
    details$title <- deployment$title
  }

  result <- POST_JSON(
    client$service,
    client$authInfo,
    v1_url("content"),
    details
  )
  list(
    id = result$id,
    guid = result$guid,
    url = result$content_url,
    # Include dashboard_url so we can open it or logs path after deploy
    dashboard_url = result$dashboard_url
  )
}

#' @export
findContent.connectClient <- function(client, deployment, quiet) {
  application <- getApplication(client, deployment$appId)
  taskComplete(quiet, "Found content {.url {application$url}}")
  application
}

#' @export
prepareContent.connectClient <- function(
  client,
  application,
  deployment,
  appMetadata,
  appVisibility,
  isNewContent,
  upload,
  quiet
) {
  envVars <- deployment$envVars
  if (length(envVars) > 0) {
    taskStart(quiet, "Updating environment variables {envVars}...")
    connectSetEnvVars(client, application$guid, envVars)
    taskComplete(quiet, "Environment variables updated")
  }
  application
}

#' @export
activateContent.connectClient <- function(client, application, bundle, quiet) {
  task <- connectDeployApplication(client, application, bundle$id)
  response <- waitForTask(client, task$task_id, quiet)
  list(
    succeeded = is.null(response$code) || response$code == 0,
    url = application$url,
    error = response$error
  )
}

#' @export
waitForTask.connectClient <- function(client, taskId, quiet = FALSE) {
  path <- v1_url("tasks", taskId)
  query <- list(first = 0, wait = 1)

  while (TRUE) {
    # ick, manual url construction
    queryString <- paste(names(query), query, sep = "=", collapse = "&")
    url <- paste0(path, "?", queryString)

    response <- GET(client$service, client$authInfo, url)

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
}

#' @export
serverDisplayName.connectClient <- function(client) {
  "Posit Connect"
}

#' @export
supportsEnvVars.connectClient <- function(client) {
  TRUE
}

#' @export
supportsEnvVarManagement.connectClient <- function(client) {
  TRUE
}

# Connect needs a minimum version for Node.js content, which
# `checkConnectSupportsNodejs()` checks.
#' @export
supportsNodejs.connectClient <- function(client) {
  TRUE
}

#' @export
usesPasswordFile.connectClient <- function(client) {
  FALSE
}

#' @export
supportsOptionalInviteEmail.connectClient <- function(client) {
  FALSE
}

#' @export
redactsUserEmails.connectClient <- function(client) {
  FALSE
}

#' @export
requiresUpload.connectClient <- function(client) {
  TRUE
}

#' @export
pythonEnabledByDefault.connectClient <- function(client) {
  TRUE
}

# Connect ignores `appVisibility`, so every value is accepted.
#' @export
validateVisibility.connectClient <- function(
  client,
  appVisibility,
  error_call
) {
  invisible()
}

#' @export
supportsMetadataSync.connectClient <- function(client) {
  TRUE
}

#' @export
staticRmdNeedsShiny.connectClient <- function(client) {
  FALSE
}

#' @export
addsUtmParameters.connectClient <- function(client) {
  FALSE
}

# All callers only need $id and $username, which registerAccount() writes to a
# .dcf file. /v1/user/ does not include $id, so this uses the unversioned URL.
#' @export
currentUser.connectClient <- function(client) {
  GET(client$service, client$authInfo, unversioned_url("users", "current"))
}

# rsconnect does not manage the users of Connect content.
#' @export
resolveContentTarget.connectClient <- function(
  client,
  accountDetails,
  appDir,
  appName,
  contentId = NULL
) {
  abortUserManagementUnsupported(client)
}

#' @export
listCollaborators.connectClient <- function(client, applicationId) {
  abortUserManagementUnsupported(client)
}

#' @export
listInvitations.connectClient <- function(client, applicationId) {
  abortUserManagementUnsupported(client)
}

#' @export
inviteApplicationUser.connectClient <- function(
  client,
  applicationId,
  email,
  sendEmail = NULL,
  emailMessage = NULL
) {
  abortUserManagementUnsupported(client)
}

#' @export
removeApplicationUser.connectClient <- function(
  client,
  applicationId,
  userId
) {
  abortUserManagementUnsupported(client)
}

#' @export
resendApplicationInvitation.connectClient <- function(
  client,
  invitationId,
  regenerate = FALSE
) {
  abortUserManagementUnsupported(client)
}

#' @export
applicationsTable.connectClient <- function(client, accountDetails) {
  serverUrl <- serverInfo(accountDetails$server)$url
  apps <- listApplications(client, accountDetails$accountId)
  rows <- lapply(apps, function(x) {
    data.frame(
      id = x$id,
      name = x$name,
      title = x$title %||% NA_character_,
      url = x$url,
      status = x$build_status,
      created_time = x$created_time,
      updated_time = x$last_deployed_time,
      guid = x$guid,
      size = NA,
      instances = NA,
      config_url = connectDashboardUrl(serverUrl, x$id),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

connectServerSettings <- function(client) {
  GET(client$service, client$authInfo, unversioned_url("server_settings"))
}

connectAddToken <- function(client, token) {
  POST_JSON(client$service, client$authInfo, unversioned_url("tokens"), token)
}

connectDeployApplication <- function(client, application, bundleId = NULL) {
  path <- v1_url("content", application$guid, "deploy")
  POST_JSON(
    client$service,
    client$authInfo,
    path,
    json = list(bundle_id = bundleId)
  )
}

# https://docs.posit.co/connect/api/#get-/v1/content/{guid}/environment
connectGetEnvVars <- function(client, guid) {
  path <- v1_url("content", guid, "environment")
  as.character(unlist(GET(client$service, client$authInfo, path, list())))
}

connectSetEnvVars <- function(client, guid, vars) {
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
  PATCH_JSON(client$service, client$authInfo, path, body)
}

getSnowflakeAuthToken <- function(url, snowflakeConnectionName) {
  parsedURL <- parseHttpUrl(url)
  ingressURL <- parsedURL$host

  # Detect when we're running in the Deploy pane of RStudio and enable
  # "interactive" temporarily so that external browser authentication is
  # permitted.
  if (rstudioapi::isBackgroundJob()) {
    rlang::local_options(rlang_interactive = TRUE)
  }

  token <- snowflakeauth::snowflake_credentials(
    snowflakeauth::snowflake_connection(snowflakeConnectionName),
    spcs_endpoint = ingressURL
  )

  token
}

# Gets the default Snowflake connection name if (1) it exists; and (2) it seems
# to match the server URL.
getDefaultSnowflakeConnectionName <- function(url) {
  connection <- tryCatch(
    snowflakeauth::snowflake_connection(),
    error = function(e) {
      cli::cli_abort(
        c(
          "No default {.arg snowflakeConnectionName}.",
          i = "Provide {.arg snowflakeConnectionName} explicitly."
        ),
        parent = e
      )
    }
  )

  # Validate that the default connection seems to match the account hosting the
  # Connect server.
  parsedURL <- parseHttpUrl(url)
  serverAccount <- extractSnowflakeAccount(parsedURL$host)
  normalizedAccount <- gsub("_", "-", connection$account, fixed = TRUE)
  if (!identical(normalizedAccount, serverAccount)) {
    cli::cli_abort(c(
      "The default Snowflake connection account {.str {connection$account}} does
       not appear to match the Connect server.",
      i = "Pass {.arg snowflakeConnectionName} to use a different connection."
    ))
  }

  connectionName <- connection$name
  if (is.null(connectionName) || !nzchar(connectionName)) {
    # This should never happen.
    cli::cli_abort(c(
      "The Snowflake connection has an empty or missing name field.",
      i = "Provide {.arg snowflakeConnectionName} explicitly."
    ))
  }

  connectionName
}

# Extract account name from an SPCS hostname.
extractSnowflakeAccount <- function(hostname) {
  # For SPCS (including privatelink) URLs, there is some alphanumeric prefix
  # followed by the hyphenated form of the account, followed by the Snowflake or
  # Snowflake Computing domain, e.g. "bf2oiajb-testorg-testaccount.snowflakecomputing.app".
  gsub(
    "([^-]+)-([^\\.]+)(|\\.privatelink)\\.(snowflakecomputing|snowflake)\\.app$",
    "\\2\\3",
    hostname
  )
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

# Construct a URL for the in-app view of a content item
connectDashboardUrl <- function(serverUrl, contentGuid) {
  prefix <- sub("/__api__$", "", serverUrl)
  paste(prefix, "connect/#/apps", contentGuid, sep = "/")
}

stripConnectTimestamps <- function(messages) {
  # Strip timestamps, if found
  timestamp_re <- "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{3,} "
  gsub(timestamp_re, "", messages)
}
