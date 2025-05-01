# Docs: https://build.posit.it/job/hostedapps/job/lucid-pipeline/job/main/API/

getCurrentProjectId <- function(service, authInfo) {
  currentApplicationId <- Sys.getenv("LUCID_APPLICATION_ID")
  if (currentApplicationId != "") {
    path <- paste0("/applications/", currentApplicationId)
    current_application <- GET(service, authInfo, path)
    return(current_application$content_id)
  }
}

cloudClient <- function(service, authInfo) {
  list(
    status = function() {
      GET(service, authInfo, "/internal/status")
    },

    service = function() {
      "posit.cloud"
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
      path <- paste0("/accounts/", accountId, "/usage/", usageType, "/")
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
      path <- paste0("/bundles/", bundleId)
      GET(service, authInfo, path)
    },

    updateBundleStatus = function(bundleId, status) {
      path <- paste0("/bundles/", bundleId, "/status")
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

    listApplications = function(accountId, filters = list()) {
      path <- "/applications/"
      query <- paste(
        filterQuery(
          c("account_id", "type", names(filters)),
          c(accountId, "connect", unname(filters))
        ),
        collapse = "&"
      )
      listRequest(service, authInfo, path, query, "applications")
    },

    getApplication = function(outputOrApplicationId, deploymentRecordVersion) {
      # The IDE doesn't know whether the id is for a content or an application, so we
      # support both.
      if (is.na(deploymentRecordVersion)) {
        # In pre-versioned dcf files, outputOrApplicationId is the id of the application.
        # TODO: consider removing support for this case a year after the release of 1.0.0
        path <- paste0("/applications/", outputOrApplicationId)
        application <- GET(service, authInfo, path)

        output_id <- application$output_id %||% application$content_id

        path <- paste0("/outputs/", output_id)
        output <- GET(service, authInfo, path)
      } else if (deploymentRecordVersion == "unknown") {
        handleError <- function(err) {
          path <- paste0("/applications/", outputOrApplicationId)
          application <- GET(service, authInfo, path)

          output_id <- application$output_id %||% application$content_id

          path <- paste0("/outputs/", output_id)
          output <- GET(service, authInfo, path)

          list(application = application, output = output)
        }
        applicationAndOutput <- tryCatch(
          {
            path <- paste0("/outputs/", outputOrApplicationId)
            output <- GET(service, authInfo, path)

            path <- paste0("/applications/", output$source_id)
            application <- GET(service, authInfo, path)

            list(application = application, output = output)
          },
          rsconnect_http_403 = handleError,
          rsconnect_http_404 = handleError
        )
        application <- applicationAndOutput$application
        output <- applicationAndOutput$output
      } else {
        # from dcf version >= 1, outputOrApplicationId is the id of the output.
        path <- paste0("/outputs/", outputOrApplicationId)
        output <- GET(service, authInfo, path)

        path <- paste0("/applications/", output$source_id)
        application <- GET(service, authInfo, path)
        application
      }

      # if the output is trashed or archived, restore it to the active state
      if (output$state == "trashed" || output$state == "archived") {
        json <- list()
        json$state <- "active"
        PATCH_JSON(service, authInfo, paste0("/outputs/", output$id), json)
      }

      # Each redeployment of a static output creates a new application. Since
      # those applications can be deleted, it's more reliable to reference
      # outputs by their own id instead of the applications'.
      application$application_id <- application$id
      application$id <- output$id
      application$url <- output$url
      application$name <- output$name
      application
    },

    getApplicationMetrics = function(
      applicationId,
      series,
      metrics,
      from = NULL,
      until = NULL,
      interval = NULL
    ) {
      path <- paste0("/applications/", applicationId, "/metrics/", series, "/")
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

    getLogs = function(applicationId, entries = 50) {
      path <- paste0("/applications/", applicationId, "/logs")
      query <- paste0("count=", entries, "&tail=0")
      GET(service, authInfo, path, query)
    },

    createApplication = function(
      name,
      title,
      template,
      accountId,
      appMode,
      contentCategory = NULL,
      spaceId = NULL
    ) {
      json <- list()
      json$name <- name
      json$application_type <- if (
        appMode %in% c("rmd-static", "quarto-static", "static")
      ) {
        "static"
      } else {
        "connect"
      }
      if (appMode %in% c("rmd-static", "quarto-static")) {
        json$render_by <- "server"
      }

      currentProjectId <- getCurrentProjectId(service, authInfo)
      # in case the source cloud project is a temporary copy, there is no
      # content id. The output will be published without a space id.
      if (!is.null(currentProjectId)) {
        json$project <- currentProjectId

        path <- paste0("/content/", currentProjectId)
        currentProject <- GET(service, authInfo, path)
        json$space <- currentProject$space_id
      }

      json$content_category <- contentCategory

      if (is.null(currentProjectId) && !is.null(spaceId)) {
        json$space <- spaceId
      }

      output <- POST_JSON(service, authInfo, "/outputs", json)
      path <- paste0("/applications/", output$source_id)
      application <- GET(service, authInfo, path)
      list(
        id = output$id,
        application_id = application$id,
        url = output$url
      )
    },

    listApplicationProperties = function(applicationId) {
      path <- paste0("/applications/", applicationId, "/properties/")
      GET(service, authInfo, path)
    },

    setApplicationProperty = function(
      applicationId,
      propertyName,
      propertyValue,
      force = FALSE
    ) {
      path <- paste0(
        "/applications/",
        applicationId,
        "/properties/",
        propertyName
      )
      v <- list()
      v$value <- propertyValue
      query <- paste0("force=", if (force) "1" else "0")
      PUT_JSON(service, authInfo, path, v, query)
    },

    unsetApplicationProperty = function(
      applicationId,
      propertyName,
      force = FALSE
    ) {
      path <- paste0(
        "/applications/",
        applicationId,
        "/properties/",
        propertyName
      )
      query <- paste0("force=", if (force) "1" else "0")
      DELETE(service, authInfo, path, query)
    },

    uploadApplication = function(applicationId, bundlePath) {
      path <- paste0("/applications/", applicationId, "/upload")
      POST(
        service,
        authInfo,
        path,
        contentType = "application/x-gzip",
        file = bundlePath
      )
    },

    createRevision = function(application, contentCategory) {
      path <- paste0("/outputs/", application$id, "/revisions")
      json <- list(content_category = contentCategory)
      revision <- POST_JSON(service, authInfo, path, json)
      revision$application_id
    },

    deployApplication = function(application, bundleId = NULL, spaceId = NULL) {
      currentProjectId <- getCurrentProjectId(service, authInfo)
      if (!is.null(currentProjectId)) {
        PATCH_JSON(
          service,
          authInfo,
          paste0("/outputs/", application$id),
          list(project = currentProjectId)
        )
      }

      if (!is.null(spaceId)) {
        PATCH_JSON(
          service,
          authInfo,
          paste0("/outputs/", application$id),
          list(space = spaceId)
        )
      }

      path <- paste0("/applications/", application$application_id, "/deploy")
      json <- list()
      if (length(bundleId) > 0 && nzchar(bundleId)) {
        json$bundle <- as.numeric(bundleId)
      } else {
        json$rebuild <- FALSE
      }
      POST_JSON(service, authInfo, path, json)
    },

    terminateApplication = function(applicationId) {
      path <- paste0("/applications/", applicationId, "/terminate")
      POST(service, authInfo, path)
    },

    purgeApplication = function(applicationId) {
      path <- paste0("/applications/", applicationId, "/purge")
      POST(service, authInfo, path)
    },

    inviteApplicationUser = function(
      applicationId,
      email,
      invite_email = NULL,
      invite_email_message = NULL
    ) {
      path <- paste0("/applications/", applicationId, "/authorization/users")
      json <- list()
      json$email <- email
      if (!is.null(invite_email)) {
        json$invite_email <- invite_email
      }
      if (!is.null(invite_email_message)) {
        json$invite_email_message <- invite_email_message
      }
      POST_JSON(service, authInfo, path, json)
    },

    addApplicationUser = function(applicationId, userId) {
      path <- paste0(
        "/applications/",
        applicationId,
        "/authorization/users/",
        userId
      )
      PUT(service, authInfo, path, NULL)
    },

    removeApplicationUser = function(applicationId, userId) {
      path <- paste0(
        "/applications/",
        applicationId,
        "/authorization/users/",
        userId
      )
      DELETE(service, authInfo, path)
    },

    listApplicationAuthorization = function(applicationId) {
      path <- paste0("/applications/", applicationId, "/authorization")
      listRequest(service, authInfo, path, NULL, "authorization")
    },

    listApplicationUsers = function(applicationId) {
      path <- paste0("/applications/", applicationId, "/authorization/users")
      listRequest(service, authInfo, path, NULL, "users")
    },

    listApplicationGroups = function(applicationId) {
      path <- paste0("/applications/", applicationId, "/authorization/groups")
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
      path <- paste0("/tasks/", taskId)
      GET(service, authInfo, path)
    },

    getTaskLogs = function(taskId) {
      path <- paste0("/tasks/", taskId, "/logs/")
      GET(service, authInfo, path)
    },

    waitForTask = function(taskId, quiet = FALSE) {
      if (!quiet) {
        cat("Waiting for task: ", taskId, "\n", sep = "")
      }

      path <- paste0("/tasks/", taskId)

      lastStatus <- NULL
      while (TRUE) {
        # check status
        status <- GET(service, authInfo, path)

        # display status to the user if it changed
        if (!identical(lastStatus, status$description)) {
          if (!quiet)
            cat("  ", status$status, ": ", status$description, "\n", sep = "")
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
}
