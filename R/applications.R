
#' List Deployed Applications
#'
#' List all applications currently deployed for a given account.
#' @param account Account name. If a single account is registered on the system
#'   then this parameter can be omitted.
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @return
#' Returns a data frame with the following columns:
#' \tabular{ll}{
#' `id`         \tab Application unique id \cr
#' `name`       \tab Name of application \cr
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
  accountDetails <- accountInfo(resolveAccount(account, server), server)
  serverDetails <- serverInfo(accountDetails$server)
  client <- clientForAccount(accountDetails)

  isConnect <- isConnectInfo(accountInfo = accountDetails)

  # retrieve applications
  apps <- client$listApplications(accountDetails$accountId)

  keep <- if (isConnect) {
    c(
      'id',
      'name',
      'url',
      'build_status',
      'created_time',
      'last_deployed_time',
      'guid'
    )
  } else {
    c(
      'id',
      'name',
      'url',
      'status',
      'created_time',
      'updated_time',
      'deployment'
    )
  }
  # extract the subset of fields we're interested in
  res <- lapply(apps, `[`, keep)

  res <- if (isConnect) {
    lapply(res, function(x){
      # set size and instance to NA since Connect doesn't return this info
      x$size <- NA
      x$instances <- NA
      x
    })
  } else {
    lapply(res, function(x) {
      # promote the size and instance data to first-level fields
      x$size <- x$deployment$properties$application.instances.template
      if (is.null(x$size))
        x$size <- NA
      x$instances <- x$deployment$properties$application.instances.count
      if (is.null(x$instances))
        x$instances <- NA
      x$deployment <- NULL
      x$guid <- NA
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
      row$config_url <- paste("https://www.shinyapps.io/admin/#/application", row$id, sep = "/")
    }
    row
  })

  # convert to data frame
  rbindWithoutFactors <- function(...){
    rbind.data.frame(..., stringsAsFactors = FALSE)
  }
  res <- do.call(rbindWithoutFactors, res)

  # Ensure the Connect and ShinyApps.io data frames have same column names
  idx <- match("last_deployed_time", names(res))
  if(!is.na(idx)) names(res)[idx] <- "updated_time"

  idx <- match("build_status", names(res))
  if (!is.na(idx)) names(res)[idx] <- "status"

  return(res)
}

resolveApplication <- function(accountDetails, appName) {
  client <- clientForAccount(accountDetails)
  apps <- client$listApplications(accountDetails$accountId)
  for (app in apps) {
    if (identical(app$name, appName))
      return (app)
  }

  stopWithApplicationNotFound(appName)
}

stopWithApplicationNotFound <- function(appName) {
  stop(paste("No application named '", appName, "' is currently deployed",
             sep=""), call. = FALSE)
}

applicationTask <- function(taskDef, appName, account, server, quiet) {

  # get status function and display initial status
  displayStatus <- displayStatus(quiet)
  displayStatus(paste(taskDef$beginStatus, "...\n", sep=""))

  # resolve target account and application
  accountDetails <- accountInfo(resolveAccount(account, server), server)
  application <- resolveApplication(accountDetails, appName)

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
  url <- paste0(shinyappsServerInfo()$url, "/applications/", applicationId,
                "/logs?", "count=", entries, "&tail=1")
  parsed <- parseHttpUrl(url)

  # create the curl handle and perform the minimum necessary to create an
  # authenticated request. we ignore the rsconnect.http option here because only
  # curl supports the kind of streaming connection that we need.
  handle <- createCurlHandle(NULL, NULL)
  curl::handle_setopt(handle, customrequest = "GET")
  curl::handle_setheaders(handle,
    .list = signatureHeaders(authInfo, "GET", parsed$path, NULL))

  # begin the stream
  curl::curl_fetch_stream(url = url,
    fun = function(data) {
      if (skip > 0)
        skip <<- skip - 1
      else
        cat(rawToChar(data))
    }, handle = handle)
}

#' Show Application Logs
#'
#' Show the logs for a deployed ShinyApps application.
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
#' @note This function only uses the \code{libcurl} transport, and works only for
#'   ShinyApps servers.
#'
#' @export
showLogs <- function(appPath = getwd(), appFile = NULL, appName = NULL,
                     account = NULL, server = NULL, entries = 50, streaming = FALSE) {

  # determine the log target and target account info
  target <- deploymentTarget(appPath, appName, NULL, NULL, account, server)
  accountDetails <- accountInfo(target$account, target$server)
  client <- lucidClient(shinyappsServerInfo()$url, accountDetails)
  application <- getAppByName(client, accountDetails, target$appName)
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")

  if (streaming) {
    # streaming; poll for the entries directly
    skip <- 0
    repeat {
      tryCatch({
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
         if (!identical(e$message,
                  "transfer closed with outstanding read data remaining")) {
           stop(e)
         }
       })
    }
  } else {
    # if not streaming, poll for the entries directly
    logs <- client$getLogs(application$id, entries)
    cat(logs)
  }
}

#' Sync Application Metadata
#'
#' Update the metadata for requested application across all deployments
#'
#' @param appPath The path to the directory or file that was deployed.
#'
#' @note This function does not update metadata for Shiny and rpubs apps
#'
#' @export
syncAppMetadata <- function(appPath) {
  if (is.null(appPath) || !file.exists(appPath)) {
    stop("appPath is null or does not exist")
  }

  # get all of the currently known deployments
  deploys <- deployments(appPath)

  syncWindow = getOption("rsconnect.metadata.sync.hours", 24) * 3600
  now = as.numeric(Sys.time())

  for (i in 1:nrow(deploys)) {
    lastSyncTime <- deploys[i, "lastSyncTime"]

    # for legacy dcf files that don't have sync time saved yet
    if (is.null(lastSyncTime) || is.na(lastSyncTime))
      lastSyncTime <- deploys[i, "when"]

    # don't sync if within the configured time window
    if (as.numeric(lastSyncTime) + syncWindow > now) {
      next
    }

    # don't sync non-connect apps
    if (!isConnectInfo(server = deploys[i, "hostUrl"])) {
      next
    }

    account <- rsconnect::accountInfo(deploys[i, "account"],
                                      server = deploys[i, "server"])

    connect <- clientForAccount(account)

    application <- NULL

    # if the app does not exist, delete the file
    tryCatch({
      application <- connect$getApplication(deploys[i, "appId"])
    }, error = function(c) {
      message(paste("appId", deploys[i, "appId"], "no longer exists, deleting config file"))
      file.remove(deploys[i, "deploymentFile"])
      stop("Removed config file ", deploys[i, "deploymentFile"])
    })

    record <- deploymentRecord(
      name = deploys[i, "name"],
      title = application$title,
      username = deploys[i, "username"],
      account = deploys[i, "account"],
      server = deploys[i, "server"],
      hostUrl = deploys[i, "hostUrl"],
      appId = deploys[i, "appId"],
      bundleId = deploys[i, "bundleId"],
      url = application$url,
      when = deploys[i, "when"],
      lastSyncTime = now,
      metadata = list(
        asMultiple = deploys[i, "asMultiple"],
        asStatic = deploys[i, "asStatic"],
        vanity_url = application$vanity_url
      )
    )

    # update the record and save out a new config file
    writeDeploymentRecord(record, deploys[i, "deploymentFile"])
  }
}
