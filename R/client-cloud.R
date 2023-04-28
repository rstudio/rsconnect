# Docs: https://build.posit.it/job/hostedapps/job/lucid-pipeline/job/main/API/

cloudClient <- function(service, authInfo) {
  list(

    status = function() {
      GET(service, authInfo,  "/internal/status")
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

    getAccountUsage = function(accountId, usageType = "hours", applicationId = NULL,
                               from = NULL, until = NULL, interval = NULL) {
      path <- paste("/accounts/", accountId, "/usage/", usageType, "/", sep = "")
      query <- list()
      if (!is.null(applicationId))
        query$application <- applicationId
      if (!is.null(from))
        query$from <- from
      if (!is.null(until))
        query$until <- until
      if (!is.null(interval))
        query$interval <- interval
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

    createBundle = function(application, content_type, content_length, checksum) {
      json <- list()
      json$application <- application
      json$content_type <- content_type
      json$content_length <- content_length
      json$checksum <- checksum
      POST_JSON(service, authInfo, "/bundles", json)
    },

    listApplications = function(accountId, filters = list()) {
      path <- "/applications/"
      query <- paste(filterQuery(
        c("account_id", "type", names(filters)),
        c(accountId, "connect", unname(filters))
      ), collapse = "&")
      listRequest(service, authInfo, path, query, "applications")
    },

    getApplication = function(applicationId, dcfVersion) {
      if (!(is.na(dcfVersion) || is.null(dcfVersion))) {
        # On version >=1, applicationId refers to the id of the output, not the application.
        path <- paste("/outputs/", applicationId, sep = "")
        output <- GET(service, authInfo, path)

        path <- paste("/applications/", output$source_id, sep = "")
        application <- GET(service, authInfo, path)
      } else {
        # backwards compatibility for data saved with the application's id
        # TODO: remove support for this case
        path <- paste("/applications/", applicationId, sep = "")
        application <- GET(service, authInfo, path)

        output_id <- ifelse(is.null(application$output_id), application$content_id, application$output_id)

        path <- paste("/outputs/", output_id, sep = "")
        output <- GET(service, authInfo, path)
      }

      # if the output is trashed or archived, restore it to the active state
      if (output$state == "trashed" || output$state == "archived") {
        json <- list()
        json$state <- "active"
        PATCH_JSON(service, authInfo, paste("/outputs/", output$id, sep = ""), json)
      }

      # Each redeployment of a static output creates a new application. Since
      # those applications can be deleted, it's more reliable to reference
      # outputs by their own id instead of the applications'.
      application$content_id <- output$id
      application$url <- output$url
      application$name <- output$name
      application
    },

    getApplicationMetrics = function(applicationId, series, metrics, from = NULL, until = NULL, interval = NULL) {
      path <- paste("/applications/", applicationId, "/metrics/", series, "/", sep = "")
      query <- list()
      m <- paste(lapply(metrics, function(x) { paste("metric", urlEncode(x), sep = "=") }), collapse = "&")
      if (!is.null(from))
        query$from <- from
      if (!is.null(until))
        query$until <- until
      if (!is.null(interval))
        query$interval <- interval
      GET(service, authInfo, path, paste(m, queryString(query), sep = "&"))
    },

    getLogs = function(applicationId, entries = 50) {
      path <- paste0("/applications/", applicationId, "/logs")
      query <- paste0("count=", entries, "&tail=0")
      GET(service, authInfo, path, query)
    },

    createApplication = function(name, title, template, accountId, appMode) {
      json <- list()
      json$name <- name
      json$application_type <- ifelse(appMode == "static", "static", "connect")

      currentApplicationId <- Sys.getenv("LUCID_APPLICATION_ID")
      if (currentApplicationId != "") {
        path <- paste("/applications/", currentApplicationId, sep = "")
        current_application <- GET(service, authInfo, path)
        project_id <- current_application$content_id

        # in case the source cloud project is a temporary copy, there is no
        # content id. The output will be published without a space id.
        if (!is.null(project_id)) {
          path <- paste("/content/", project_id, sep = "")
          current_project <- GET(service, authInfo, path)
          json$project <- current_project$id
          json$space <- current_project$space_id
        }
      }
      output <- POST_JSON(service, authInfo, "/outputs", json)
      path <- paste("/applications/", output$source_id, sep = "")
      application <- GET(service, authInfo, path)
      application$content_id <- output$id
      # this swaps the "application url" for the "content url". So we end up redirecting to the right spot after deployment.
      application$url <- output$url
      application
    },

    listApplicationProperties = function(applicationId) {
      path <- paste("/applications/", applicationId, "/properties/", sep = "")
      GET(service, authInfo, path)
    },

    setApplicationProperty = function(applicationId, propertyName,
                                      propertyValue, force = FALSE) {
      path <- paste("/applications/", applicationId, "/properties/",
                    propertyName, sep = "")
      v <- list()
      v$value <- propertyValue
      query <- paste("force=", if (force) "1" else "0", sep = "")
      PUT_JSON(service, authInfo, path, v, query)
    },

    unsetApplicationProperty = function(applicationId, propertyName,
                                        force = FALSE) {
      path <- paste("/applications/", applicationId, "/properties/",
                    propertyName, sep = "")
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

    createRevision = function(application) {
        path <- paste("/outputs/", application$content_id, "/revisions", sep = "")
        revision <- POST_JSON(service, authInfo, path, data.frame())
        revision$application_id
    },

    deployApplication = function(application, bundleId = NULL) {
      path <- paste("/applications/", application$id, "/deploy", sep = "")
      json <- list()
      if (length(bundleId) > 0 && nzchar(bundleId))
        json$bundle <- as.numeric(bundleId)
      else
        json$rebuild <- FALSE
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

    inviteApplicationUser = function(applicationId, email,
                                     invite_email = NULL, invite_email_message = NULL) {
      path <- paste("/applications/", applicationId, "/authorization/users",
                    sep = "")
      json <- list()
      json$email <- email
      if (!is.null(invite_email))
        json$invite_email <- invite_email
      if (!is.null(invite_email_message))
        json$invite_email_message <- invite_email_message
      POST_JSON(service, authInfo, path, json)
    },

    addApplicationUser = function(applicationId, userId) {
      path <- paste("/applications/", applicationId, "/authorization/users/",
                    userId, sep = "")
      PUT(service, authInfo, path, NULL)
    },

    removeApplicationUser = function(applicationId, userId) {
      path <- paste("/applications/", applicationId, "/authorization/users/",
                    userId, sep = "")
      DELETE(service, authInfo, path)
    },

    listApplicationAuthorization = function(applicationId) {
      path <- paste("/applications/", applicationId, "/authorization",
                    sep = "")
      listRequest(service, authInfo, path, NULL, "authorization")
    },

    listApplicationUsers = function(applicationId) {
      path <- paste("/applications/", applicationId, "/authorization/users",
                    sep = "")
      listRequest(service, authInfo, path, NULL, "users")
    },

    listApplicationGroups = function(applicationId) {
      path <- paste("/applications/", applicationId, "/authorization/groups",
                    sep = "")
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
