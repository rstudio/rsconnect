shinyAppsClient <- function(service, authInfo) {
  self <- list(
    # The connection identity. Methods read these to make requests.
    service = service,
    authInfo = authInfo,

    status = function() {
      GET(service, authInfo, "/internal/status")
    },

    currentUser = function() {
      GET(service, authInfo, "/users/current/")
    },

    accountsForUser = function(userId) {
      path <- "/accounts/"
      query <- ""
      listRequest(service, authInfo, path, query, "accounts")
    },

    getAccountUsage = function(
      accountId,
      usageType = "hours",
      applicationId = NULL,
      from = NULL,
      until = NULL,
      interval = NULL
    ) {
      path <- paste(
        "/accounts/",
        accountId,
        "/usage/",
        usageType,
        "/",
        sep = ""
      )
      query <- list()
      if (!is.null(applicationId)) {
        query$application <- applicationId
      }
      if (!is.null(from)) {
        query$from <- from
      }
      if (!is.null(until)) {
        query$until <- until
      }
      if (!is.null(interval)) {
        query$interval <- interval
      }
      GET(service, authInfo, path, queryString(query))
    },

    getBundle = function(bundleId) {
      path <- paste("/bundles/", bundleId, sep = "")
      GET(service, authInfo, path)
    },

    updateBundleStatus = function(bundleId, status) {
      path <- paste("/bundles/", bundleId, "/status", sep = "")
      json <- list()
      json$status <- status
      POST_JSON(service, authInfo, path, json)
    },

    createBundle = function(
      application,
      content_type,
      content_length,
      checksum
    ) {
      json <- list()
      json$application <- application
      json$content_type <- content_type
      json$content_length <- content_length
      json$checksum <- checksum
      POST_JSON(service, authInfo, "/bundles", json)
    },

    getApplicationMetrics = function(
      applicationId,
      series,
      metrics,
      from = NULL,
      until = NULL,
      interval = NULL
    ) {
      path <- paste(
        "/applications/",
        applicationId,
        "/metrics/",
        series,
        "/",
        sep = ""
      )
      query <- list()
      m <- paste(
        lapply(metrics, function(x) {
          paste("metric", urlEncode(x), sep = "=")
        }),
        collapse = "&"
      )
      if (!is.null(from)) {
        query$from <- from
      }
      if (!is.null(until)) {
        query$until <- until
      }
      if (!is.null(interval)) {
        query$interval <- interval
      }
      GET(service, authInfo, path, paste(m, queryString(query), sep = "&"))
    },

    getLogs = function(applicationId, entries = 50, format = NULL) {
      path <- paste0("/applications/", applicationId, "/logs")
      query <- paste0("count=", entries, "&tail=0")
      if (!is.null(format)) {
        # format=json returns a structured response.
        query <- paste0(query, "&format=", format)
      }
      GET(service, authInfo, path, query)
    },

    listApplicationProperties = function(applicationId) {
      path <- paste("/applications/", applicationId, "/properties/", sep = "")
      GET(service, authInfo, path)
    },

    setApplicationProperty = function(
      applicationId,
      propertyName,
      propertyValue,
      force = FALSE
    ) {
      path <- paste(
        "/applications/",
        applicationId,
        "/properties/",
        propertyName,
        sep = ""
      )
      v <- list()
      v$value <- propertyValue
      query <- paste("force=", if (force) "1" else "0", sep = "")
      PUT_JSON(service, authInfo, path, v, query)
    },

    unsetApplicationProperty = function(
      applicationId,
      propertyName,
      force = FALSE
    ) {
      path <- paste(
        "/applications/",
        applicationId,
        "/properties/",
        propertyName,
        sep = ""
      )
      query <- paste("force=", if (force) "1" else "0", sep = "")
      DELETE(service, authInfo, path, query)
    },

    uploadApplication = function(applicationId, bundlePath) {
      path <- paste("/applications/", applicationId, "/upload", sep = "")
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
      if (length(bundleId) > 0 && nzchar(bundleId)) {
        json$bundle <- as.numeric(bundleId)
      } else {
        json$rebuild <- FALSE
      }
      POST_JSON(service, authInfo, path, json)
    },

    terminateApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, "/terminate", sep = "")
      POST(service, authInfo, path)
    },

    purgeApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, "/purge", sep = "")
      POST(service, authInfo, path)
    },

    addApplicationUser = function(applicationId, userId) {
      path <- paste(
        "/applications/",
        applicationId,
        "/authorization/users/",
        userId,
        sep = ""
      )
      PUT(service, authInfo, path, NULL)
    },

    listApplicationAuthorization = function(applicationId) {
      path <- paste("/applications/", applicationId, "/authorization", sep = "")
      listRequest(service, authInfo, path, NULL, "authorization")
    },

    listApplicationUsers = function(applicationId) {
      path <- paste(
        "/applications/",
        applicationId,
        "/authorization/users",
        sep = ""
      )
      listRequest(service, authInfo, path, NULL, "users")
    },

    listApplicationGroups = function(applicationId) {
      path <- paste(
        "/applications/",
        applicationId,
        "/authorization/groups",
        sep = ""
      )
      listRequest(service, authInfo, path, NULL, "groups")
    },

    listApplicationInvitations = function(applicationId) {
      path <- "/invitations/"
      query <- paste(filterQuery("app_id", applicationId), collapse = "&")
      listRequest(service, authInfo, path, query, "invitations")
    },

    listTasks = function(accountId, filters = NULL) {
      if (is.null(filters)) {
        filters <- vector()
      }
      path <- "/tasks/"
      filters <- c(filterQuery("account_id", accountId), filters)
      query <- paste(filters, collapse = "&")
      listRequest(service, authInfo, path, query, "tasks", max = 100)
    },

    getTaskInfo = function(taskId) {
      path <- paste("/tasks/", taskId, sep = "")
      GET(service, authInfo, path)
    },

    getTaskLogs = function(taskId) {
      path <- paste("/tasks/", taskId, "/logs/", sep = "")
      GET(service, authInfo, path)
    },

    waitForTask = function(taskId, quiet = FALSE) {
      if (!quiet) {
        cat("Waiting for task: ", taskId, "\n", sep = "")
      }

      path <- paste("/tasks/", taskId, sep = "")

      lastStatus <- NULL
      while (TRUE) {
        # check status
        status <- GET(service, authInfo, path)

        # display status to the user if it changed
        if (!identical(lastStatus, status$description)) {
          if (!quiet) {
            cat("  ", status$status, ": ", status$description, "\n", sep = "")
          }
          lastStatus <- status$description
        }

        # are we finished? (note: this codepath is the only way to exit this function)
        if (status$finished) {
          if (identical(status$status, "success")) {
            return(NULL)
          } else {
            # always show task log on error
            cli::cat_rule("Begin Task Log", line = "#")
            taskLog(taskId, authInfo$name, authInfo$server, output = "stderr")
            cli::cat_rule("End Task Log", line = "#")
            stop(status$error, call. = FALSE)
          }
        }

        # wait for 1 second before polling again
        Sys.sleep(1)
      }
    }
  )
  structure(self, class = c("shinyAppsClient", "rsconnectClient"))
}

#' @export
uploadBundle.shinyAppsClient <- function(client, application, bundlePath) {
  # Step 1. Create presigned URL and register pending bundle.
  bundleSize <- file.info(bundlePath)$size
  bundle <- client$createBundle(
    application$application_id,
    content_type = "application/x-tar",
    content_length = bundleSize,
    checksum = fileMD5(bundlePath)
  )

  # Step 2. Upload the bundle to the presigned URL.
  if (!putPresignedBundle(bundle, bundleSize, bundlePath)) {
    stop("Could not upload file.")
  }

  # Step 3. Set the bundle status to ready.
  response <- client$updateBundleStatus(bundle$id, status = "ready")

  # Step 4. Get the updated bundle after the status change.
  client$getBundle(bundle$id)
}

#' @export
listApplications.shinyAppsClient <- function(
  client,
  accountId,
  filters = list()
) {
  path <- "/applications/"
  query <- paste(
    filterQuery(
      c("account_id", "type", names(filters)),
      c(accountId, "shiny", unname(filters))
    ),
    collapse = "&"
  )
  listRequest(client$service, client$authInfo, path, query, "applications")
}

#' @export
getApplication.shinyAppsClient <- function(client, applicationId) {
  path <- paste("/applications/", applicationId, sep = "")
  application <- GET(client$service, client$authInfo, path)
  application$application_id <- application$id
  application
}

#' @export
createContent.shinyAppsClient <- function(
  client,
  deployment,
  accountDetails,
  appMetadata,
  appVisibility = NULL
) {
  json <- list(
    name = deployment$name,
    template = "shiny",
    account = as.numeric(accountDetails$accountId)
  )
  application <- POST_JSON(
    client$service,
    client$authInfo,
    "/applications/",
    json
  )
  list(
    id = application$id,
    application_id = application$id,
    url = application$url
  )
}

#' @export
findContent.shinyAppsClient <- function(client, deployment, quiet) {
  application <- getApplication(client, deployment$appId)
  taskComplete(quiet, "Found content {.url {application$url}}")
  application
}

# The visibility must be set before the deploy. An application with no
# visibility property is public.
needsVisibilityChange <- function(application, appVisibility = NULL) {
  if (is.null(appVisibility)) {
    return(FALSE)
  }

  cur <- application$deployment$properties$application.visibility
  if (is.null(cur)) {
    cur <- "public"
  }
  cur != appVisibility
}

#' @export
prepareContent.shinyAppsClient <- function(
  client,
  application,
  deployment,
  appMetadata,
  appVisibility,
  isNewContent,
  upload,
  quiet
) {
  if (needsVisibilityChange(application, appVisibility)) {
    taskStart(quiet, "Setting visibility to {appVisibility}...")
    client$setApplicationProperty(
      application$id,
      "application.visibility",
      appVisibility
    )
    taskComplete(quiet, "Visibility updated")
  }
  application
}

#' @export
activateContent.shinyAppsClient <- function(
  client,
  application,
  bundle,
  quiet
) {
  # A deploy without an upload deploys the current bundle again.
  bundle <- bundle %||% application$deployment$bundle
  task <- client$deployApplication(application, bundle$id)
  response <- client$waitForTask(task$task_id, quiet)
  list(
    succeeded = is.null(response$code) || response$code == 0,
    url = application$url,
    error = response$error
  )
}

#' @export
serverDisplayName.shinyAppsClient <- function(client) {
  "shinyapps.io"
}

#' @export
supportsEnvVars.shinyAppsClient <- function(client) {
  FALSE
}

#' @export
supportsEnvVarManagement.shinyAppsClient <- function(client) {
  FALSE
}

#' @export
supportsNodejs.shinyAppsClient <- function(client) {
  FALSE
}

#' @export
usesPasswordFile.shinyAppsClient <- function(client) {
  TRUE
}

#' @export
supportsOptionalInviteEmail.shinyAppsClient <- function(client) {
  TRUE
}

#' @export
redactsUserEmails.shinyAppsClient <- function(client) {
  FALSE
}

#' @export
requiresUpload.shinyAppsClient <- function(client) {
  FALSE
}

#' @export
pythonEnabledByDefault.shinyAppsClient <- function(client) {
  FALSE
}

#' @export
validateVisibility.shinyAppsClient <- function(
  client,
  appVisibility,
  error_call
) {
  arg_match(appVisibility, c("private", "public"), error_call = error_call)
  invisible()
}

#' @export
supportsMetadataSync.shinyAppsClient <- function(client) {
  TRUE
}

# shinyapps.io serves all content from a Shiny process, so it cannot serve
# `"rmd-static"` content.
#' @export
staticRmdNeedsShiny.shinyAppsClient <- function(client) {
  TRUE
}

#' @export
addsUtmParameters.shinyAppsClient <- function(client) {
  FALSE
}

#' @export
applicationsTable.shinyAppsClient <- function(client, accountDetails) {
  apps <- listApplications(client, accountDetails$accountId)
  rows <- lapply(apps, function(x) {
    properties <- x$deployment$properties
    data.frame(
      id = x$id,
      name = x$name,
      url = x$url,
      status = x$status,
      created_time = x$created_time,
      updated_time = x$updated_time,
      size = properties$application.instances.template %||% NA,
      instances = properties$application.instances.count %||% NA,
      guid = NA,
      title = NA_character_,
      config_url = paste0(
        "https://www.shinyapps.io/admin/#/application/",
        x$id
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @export
resolveContentTarget.shinyAppsClient <- function(
  client,
  accountDetails,
  appDir,
  appName,
  contentId = NULL
) {
  if (!is.null(contentId)) {
    cli::cli_abort(c(
      "{.arg contentId} is only supported on Posit Connect Cloud.",
      i = "On shinyapps.io, identify the application with {.arg appName}."
    ))
  }
  application <- resolveApplication(
    client,
    accountDetails,
    appName %||% basename(appDir)
  )
  list(id = application$id, deploymentFile = NULL)
}

#' @export
listCollaborators.shinyAppsClient <- function(client, applicationId) {
  res <- client$listApplicationAuthorization(applicationId)
  rows <- lapply(res, function(x) {
    id <- as.character(x$user$id %||% NA_character_)
    email <- as.character(x$user$email %||% NA_character_)
    checkCollaboratorRecord(client, id, email)
    data.frame(
      id = id,
      email = email,
      account = as.character(x$account %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    return(data.frame(
      id = character(),
      email = character(),
      account = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' @export
inviteApplicationUser.shinyAppsClient <- function(
  client,
  applicationId,
  email,
  sendEmail = NULL,
  emailMessage = NULL
) {
  path <- paste(
    "/applications/",
    applicationId,
    "/authorization/users",
    sep = ""
  )
  json <- list()
  json$email <- email
  if (!is.null(sendEmail)) {
    json$invite_email <- sendEmail
  }
  if (!is.null(emailMessage)) {
    json$invite_email_message <- emailMessage
  }
  POST_JSON(client$service, client$authInfo, path, json)
}

#' @export
removeApplicationUser.shinyAppsClient <- function(
  client,
  applicationId,
  userId
) {
  path <- paste(
    "/applications/",
    applicationId,
    "/authorization/users/",
    userId,
    sep = ""
  )
  DELETE(client$service, client$authInfo, path, NULL)
}

#' @export
resendApplicationInvitation.shinyAppsClient <- function(
  client,
  invitationId,
  regenerate = FALSE
) {
  path <- paste("/invitations/", invitationId, "/send", sep = "")
  json <- list()
  json$regenerate <- regenerate
  POST_JSON(client$service, client$authInfo, path, json)
}

#' @export
listInvitations.shinyAppsClient <- function(client, applicationId) {
  res <- client$listApplicationInvitations(applicationId)
  rows <- lapply(res, function(x) {
    data.frame(
      id = as.character(x$id %||% NA_character_),
      email = as.character(x$email %||% NA_character_),
      link = as.character(x$link %||% NA_character_),
      expired = as.logical(x$expired %||% NA),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    return(emptyInvitations())
  }
  do.call(rbind, rows)
}

putPresignedBundle <- function(bundle, bundleSize, bundlePath) {
  presigned_service <- parseHttpUrl(bundle$presigned_url)

  headers <- list()
  headers$`Content-Type` <- "application/x-tar"
  headers$`Content-Length` <- bundleSize

  # AWS requires a base64 encoded hash
  headers$`Content-MD5` <- bundle$presigned_checksum

  # AWS is very sensitive to extra headers, because they were not signed when
  # the presigned link was made. So the lower level library is used here.
  response <- httpLibCurl(
    presigned_service$protocol,
    presigned_service$host,
    presigned_service$port,
    "PUT",
    presigned_service$path,
    headers,
    headers$`Content-Type`,
    bundlePath
  )

  response$status == 200
}
