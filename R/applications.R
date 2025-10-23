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
  # resolve account and create connect client
  accountDetails <- accountInfo(account, server)
  serverDetails <- serverInfo(accountDetails$server)
  client <- clientForAccount(accountDetails)

  if (isPositConnectCloudServer(accountDetails$server)) {
    cli::cli_abort(
      "The applications() function is not supported for Posit Connect Cloud accounts."
    )
  }

  isConnect <- isConnectServer(accountDetails$server)

  # retrieve applications
  apps <- client$listApplications(accountDetails$accountId)

  # extract the subset of fields we're interested in
  keep <- if (isConnect) {
    c(
      "id",
      "name",
      "title",
      "url",
      "build_status",
      "created_time",
      "last_deployed_time",
      "guid"
    )
  } else {
    c(
      "id",
      "name",
      "url",
      "status",
      "created_time",
      "updated_time",
      "deployment"
    )
  }
  res <- lapply(apps, `[`, keep)

  res <- if (isConnect) {
    lapply(res, function(x) {
      # set size and instance to NA since Connect doesn't return this info
      x$size <- NA
      x$instances <- NA
      x$title <- x$title %||% NA_character_
      x
    })
  } else {
    lapply(res, function(x) {
      # promote the size and instance data to first-level fields
      x$size <- x$deployment$properties$application.instances.template
      if (is.null(x$size)) {
        x$size <- NA
      }
      x$instances <- x$deployment$properties$application.instances.count
      if (is.null(x$instances)) {
        x$instances <- NA
      }
      x$deployment <- NULL
      x$guid <- NA
      x$title <- NA_character_
      x
    })
  }

  # The config URL may be provided by the server at some point, but for now
  # infer it from the account type
  res <- lapply(res, function(row) {
    if (isConnect) {
      prefix <- sub("/__api__", "", serverDetails$url)
      row$config_url <- paste(prefix, "connect/#/apps", row$id, sep = "/")
    } else {
      row$config_url <- paste(
        "https://www.shinyapps.io/admin/#/application",
        row$id,
        sep = "/"
      )
    }
    row
  })

  # convert to data frame
  res <- lapply(res, as.data.frame, stringsAsFactors = FALSE)
  res <- do.call("rbind", res)

  # Ensure the Connect and ShinyApps.io data frames have same column names
  idx <- match("last_deployed_time", names(res))
  if (!is.na(idx)) {
    names(res)[idx] <- "updated_time"
  }

  idx <- match("build_status", names(res))
  if (!is.na(idx)) {
    names(res)[idx] <- "status"
  }

  return(res)
}

# Use the API to filter applications by name and error when it does not exist.
getAppByName <- function(client, accountInfo, name, error_call = caller_env()) {
  # NOTE: returns a list with 0 or 1 elements
  app <- client$listApplications(
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
resolveApplication <- function(accountDetails, appName) {
  client <- clientForAccount(accountDetails)
  apps <- client$listApplications(accountDetails$accountId)
  for (app in apps) {
    if (identical(app$name, appName)) {
      return(app)
    }
  }

  stopWithApplicationNotFound(appName)
}

getApplication <- function(account, server, appId) {
  accountDetails <- accountInfo(account, server)
  client <- clientForAccount(accountDetails)

  withCallingHandlers(
    client$getApplication(appId, "unknown"),
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
  application <- resolveApplication(accountDetails, appName)

  # get status function and display initial status
  displayStatus <- displayStatus(quiet)
  displayStatus(paste(taskDef$beginStatus, "...\n", sep = ""))

  # perform the action
  client <- clientForAccount(accountDetails)
  task <- taskDef$action(client, application)
  client$waitForTask(task$task_id, quiet)
  displayStatus(paste(taskDef$endStatus, "\n", sep = ""))

  invisible(NULL)
}

# streams application logs from ShinyApps
streamApplicationLogs <- function(authInfo, applicationId, entries, skip) {
  # build the URL
  url <- paste0(
    serverInfo("shinyapps.io")$url,
    "/applications/",
    applicationId,
    "/logs?",
    "count=",
    entries,
    "&tail=1"
  )
  parsed <- parseHttpUrl(url)

  # create the curl handle and perform the minimum necessary to create an
  # authenticated request. we ignore the rsconnect.http option here because only
  # curl supports the kind of streaming connection that we need.
  handle <- createCurlHandle("GET")
  curl::handle_setheaders(
    handle,
    .list = signatureHeaders(authInfo, "GET", parsed$path, NULL)
  )

  # begin the stream
  curl::curl_fetch_stream(
    url = url,
    fun = function(data) {
      if (skip > 0) {
        skip <<- skip - 1
      } else {
        cat(rawToChar(data))
      }
    },
    handle = handle
  )
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
#' @param streaming Whether to stream the logs. If `TRUE`, then the
#'   function does not return; instead, log entries are written to the console
#'   as they are made, until R is interrupted. Defaults to `FALSE`.
#'
#' @note These functions only use the \code{libcurl} transport, and only work
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

  accountDetails <- accountInfo(deployment$account, deployment$server)
  client <- clientForAccount(accountDetails)
  application <- getAppByName(client, accountDetails, deployment$name)

  if (streaming) {
    # streaming; poll for the entries directly
    skip <- 0
    repeat {
      tryCatch(
        {
          streamApplicationLogs(accountDetails, application$id, entries, skip)
          # after the first fetch, we've seen all recent entries, so show
          # only new entries. unfortunately /logs/ doesn't support getting 0
          # entries, so get one and don't log it.
          entries <- 1
          skip <- 1
        },
        error = function(e) {
          # if the server times out, ignore the error; otherwise, let it
          # bubble through
          if (
            !identical(
              e$message,
              "transfer closed with outstanding read data remaining"
            )
          ) {
            stop(e)
          }
        }
      )
    }
  } else {
    # if not streaming, poll for the entries directly
    logs <- client$getLogs(application$id, entries)
    cat(logs)
  }
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
  accountDetails <- accountInfo(deployment$account, deployment$server)
  client <- clientForAccount(accountDetails)
  application <- getAppByName(client, accountDetails, deployment$name)

  payload <- client$getLogs(application$id, entries, format = "json")

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

    # don't sync if published to RPubs or Connect Cloud
    if (isRPubs(curDeploy$server)) {
      next
    } else if (isPositConnectCloudServer(curDeploy$server)) {
      next
    }

    account <- accountInfo(curDeploy$account, curDeploy$server)
    client <- clientForAccount(account)

    application <- tryCatch(
      client$getApplication(curDeploy$appId),
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
