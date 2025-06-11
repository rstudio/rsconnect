shinyAppsClient <- function(service, authInfo) {
  list(
    status = function() {
      GET(service, authInfo, "/internal/status")
    },

    service = function() {
      "shinyapps.io"
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

    listApplications = function(accountId, filters = list()) {
      path <- "/applications/"
      query <- paste(
        filterQuery(
          c("account_id", "type", names(filters)),
          c(accountId, "shiny", unname(filters))
        ),
        collapse = "&"
      )
      listRequest(service, authInfo, path, query, "applications")
    },

    getApplication = function(applicationId, deploymentRecordVersion) {
      path <- paste("/applications/", applicationId, sep = "")
      application <- GET(service, authInfo, path)
      application$application_id <- application$id
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
      # the title field is only used on connect
      json$template <- template
      json$account <- as.numeric(accountId)
      application <- POST_JSON(service, authInfo, "/applications/", json)
      list(
        id = application$id,
        application_id = application$id,
        url = application$url
      )
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

    deployApplication = function(application, bundleId = NULL, spaceId = NULL) {
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

    inviteApplicationUser = function(
      applicationId,
      email,
      invite_email = NULL,
      invite_email_message = NULL
    ) {
      path <- paste(
        "/applications/",
        applicationId,
        "/authorization/users",
        sep = ""
      )
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
      path <- paste(
        "/applications/",
        applicationId,
        "/authorization/users/",
        userId,
        sep = ""
      )
      PUT(service, authInfo, path, NULL)
    },

    removeApplicationUser = function(applicationId, userId) {
      path <- paste(
        "/applications/",
        applicationId,
        "/authorization/users/",
        userId,
        sep = ""
      )
      DELETE(service, authInfo, path, NULL)
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

    resendApplicationInvitation = function(invitationId, regenerate = FALSE) {
      path <- paste("/invitations/", invitationId, "/send", sep = "")
      json <- list()
      json$regenerate <- regenerate
      POST_JSON(service, authInfo, path, json)
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
}
