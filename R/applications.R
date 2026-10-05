#' List Deployed Applications
#'
#' @description
#' List all applications currently deployed for a given account.
#'
#' Supported servers: All servers
#'
#' @inheritParams deployApp
#' @return
#' Returns a data frame with the following columns:
#' \tabular{ll}{
#' `id`         \tab Application unique id \cr
#' `name`       \tab Name of application \cr
#' `title`       \tab Application title \cr
#' `url`        \tab URL where application can be accessed \cr
#'
#' `status`     \tab Current status of application. Valid values are `pending`,
#'                   `deploying`, `running`, `terminating`, and `terminated` \cr
#' `size`       \tab Instance size (small, medium, large, etc.) (on
#'                   ShinyApps.io) \cr
#' `instances`  \tab Number of instances (on ShinyApps.io) \cr
#' `config_url` \tab URL where application can be configured \cr
#' }
#' @note To register an account you call the [setAccountInfo()] function.
#' @examples
#' \dontrun{
#'
#' # list all applications for the default account
#' applications()
#'
#' # list all applications for a specific account
#' applications("myaccount")
#'
#' # view the list of applications in the data viewer
#' View(applications())
#' }
#' @seealso [deployApp()], [terminateApp()]
#' @family Deployment functions
#' @export
applications <- function(account = NULL, server = NULL) {
  accountDetails <- accountInfo(account, server)
  client <- clientForAccount(accountDetails)
  applicationsTable(client, accountDetails)
}

# Use the API to filter applications by name and error when it does not exist.
getAppByName <- function(client, accountInfo, name, error_call = caller_env()) {
  # NOTE: returns a list with 0 or 1 elements
  app <- listApplications(
    client,
    accountInfo$accountId,
    filters = list(name = name)
  )
  if (length(app)) {
    return(app[[1]])
  }
  cli::cli_abort(
    c(
      "No application found",
      i = "Specify the application directory, name, and/or associated account."
    ),
    call = error_call,
    class = "rsconnect_app_not_found"
  )
}

# Use the API to list all applications then filter the results client-side.
resolveApplication <- function(client, accountDetails, appName) {
  apps <- listApplications(client, accountDetails$accountId)
  for (app in apps) {
    if (identical(app$name, appName)) {
      return(app)
    }
  }

  stopWithApplicationNotFound(appName)
}

getApplicationForAccount <- function(account, server, appId) {
  accountDetails <- accountInfo(account, server)
  client <- clientForAccount(accountDetails)

  withCallingHandlers(
    getApplication(client, appId),
    rsconnect_http_404 = function(err) {
      cli::cli_abort("Can't find app with id {.str {appId}}", parent = err)
    }
  )
}

stopWithApplicationNotFound <- function(appName) {
  stop(
    paste(
      "No application named '",
      appName,
      "' is currently deployed.",
      sep = ""
    ),
    call. = FALSE
  )
}

applicationTask <- function(taskDef, appName, accountDetails, quiet) {
  # resolve target account and application
  client <- clientForAccount(accountDetails)
  application <- resolveApplication(client, accountDetails, appName)

  # get status function and display initial status
  displayStatus <- displayStatus(quiet)
  displayStatus(paste(taskDef$beginStatus, "...\n", sep = ""))

  # perform the action
  task <- taskDef$action(client, application)
  shinyappsWaitForTask(client, task$task_id, quiet)
  displayStatus(paste(taskDef$endStatus, "\n", sep = ""))

  invisible(NULL)
}

#' Application Logs
#'
#' @description
#' These functions provide access to the logs for deployed ShinyApps applications:
#'
#' * `showLogs()` displays the logs.
#' * `getLogs()` returns the logged lines.
#'
#' Supported servers: ShinyApps servers
#'
#' @param appPath The path to the directory or file that was deployed.
#' @param appFile The path to the R source file that contains the application
#'   (for single file applications).
#' @param appName The name of the application to show logs for. May be omitted
#'   if only one application deployment was made from `appPath`.
#' @param account The account under which the application was deployed. May be
#'   omitted if only one account is registered on the system.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @param entries The number of log entries to show. Defaults to 50 entries.
#' @param streaming Deprecated. Streaming logs is not currently supported
#'   as the ShinyApps.io backend no longer supports this feature. If `TRUE`,
#'   an error will be thrown. Defaults to `FALSE`.
#'
#' @note These functions only work
#'   for applications deployed to ShinyApps.io.
#'
#' @return `getLogs()` returns a data frame containing the logged lines.
#'
#' @export
showLogs <- function(
  appPath = getwd(),
  appFile = NULL,
  appName = NULL,
  account = NULL,
  server = NULL,
  entries = 50,
  streaming = FALSE
) {
  # determine the log target and target account info
  deployment <- findDeployment(
    appPath = appPath,
    appName = appName,
    server = server,
    account = account
  )

  checkShinyappsServer(deployment$server)

  if (streaming) {
    cli::cli_abort(
      c(
        "Streaming logs is not currently supported.",
        i = "The ShinyApps.io backend no longer supports the streaming API.",
        i = "Use {.arg streaming = FALSE} (the default) to retrieve recent log entries."
      )
    )
  }

  accountDetails <- accountInfo(deployment$account, deployment$server)
  client <- clientForAccount(accountDetails)
  application <- getAppByName(client, accountDetails, deployment$name)

  # Poll for the entries directly
  logs <- shinyappsGetLogs(client, application$id, entries)
  cat(logs)
}

#' @rdname showLogs
#' @export
getLogs <- function(
  appPath = getwd(),
  appFile = NULL,
  appName = NULL,
  account = NULL,
  server = NULL,
  entries = 50
) {
  # determine the log target and target account info
  deployment <- findDeployment(
    appPath = appPath,
    appName = appName,
    server = server,
    account = account
  )
  checkShinyappsServer(deployment$server)
  accountDetails <- accountInfo(deployment$account, deployment$server)
  client <- clientForAccount(accountDetails)
  application <- getAppByName(client, accountDetails, deployment$name)

  payload <- shinyappsGetLogs(client, application$id, entries, format = "json")

  # Convert to a dataframe before combining because the JSON payload has inconsistent field order
  # containing nested single-element lists.
  converted <- lapply(payload$results, as.data.frame)
  df <- do.call(rbind, converted)

  # shinyapps.io returns ns timestamps.
  df$timestamp <- as.POSIXct(df$timestamp / (1000 * 1000))

  # Return a subset of the included fields.
  result <- df[c(
    "timestamp",
    "account_id",
    "application_id",
    "message"
  )]
  result
}

#' Update deployment records
#'
#' @description
#' Update the deployment records for applications published to Posit Connect.
#' This updates application title and URL, and deletes records for deployments
#' where the application has been deleted on the server.
#'
#' Supported servers: Posit Connect servers
#'
#' @param appPath The path to the directory or file that was deployed.
#' @export
syncAppMetadata <- function(appPath = ".") {
  check_directory(appPath)

  deploys <- deployments(appPath)
  for (i in seq_len(nrow(deploys))) {
    curDeploy <- deploys[i, ]

    # RPubs has no client, so check it before the client is created
    if (isRPubs(curDeploy$server)) {
      next
    }

    account <- accountInfo(curDeploy$account, curDeploy$server)
    client <- clientForAccount(account)
    if (!supportsMetadataSync(client)) {
      next
    }

    application <- tryCatch(
      getApplication(client, curDeploy$appId),
      rsconnect_http_404 = function(c) {
        # if the app has been deleted, delete the deployment record
        file.remove(curDeploy$deploymentFile)
        cli::cli_inform(
          "Deleting deployment record for deleted app {curDeploy$appId}."
        )
        NULL
      }
    )
    if (is.null(application)) {
      next
    }

    # update the record and save out a new config file
    path <- curDeploy$deploymentFile
    curDeploy$deploymentFile <- NULL # added on read

    # remove old fields
    curDeploy$when <- NULL
    curDeploy$lastSyncTime <- NULL

    curDeploy$title <- application$title
    curDeploy$url <- application$url

    writeDeploymentRecord(curDeploy, path)
  }
}
