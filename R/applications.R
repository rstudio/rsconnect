
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
#' \code{name} \tab Name of application \cr
#' \code{url} \tab URL where application can be accessed\cr
#' \code{config_url} \tab URL where application can be configured\cr
#' \code{status} \tab Current status of application. Valid values are
#' \code{pending}, \code{deploying}, \code{running}, \code{terminating}, and
#' \code{terminated}.
#' }
#' @note To register an account you call the \link{setAccountInfo} function.
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
#' @seealso \code{\link{deployApp}}, \code{\link{terminateApp}}
#' @export
applications <- function(account = NULL, server = NULL) {

  # resolve account and create connect client
  accountDetails <- accountInfo(resolveAccount(account, server), server)
  serverDetails <- serverInfo(accountDetails$server)
  client <- clientForAccount(accountDetails)

  # retreive applications
  apps <- client$listApplications(accountDetails$accountId)

  # extract the subset of fields we're interested in
  res <- lapply(apps, `[`, c('id', 'name', 'url', 'status', 'created_time',
                             'updated_time', 'deployment'))

  # promote the size and instance data to first-level fields
  res <- lapply(res, function(x) {
    if (! is.null(x$deployment)) {
      x$size <- x$deployment$properties$application.instances.template
      x$instances <- x$deployment$properties$application.instances.count
      if (is.null(x$instances))
        x$instances <- NA
    } else {
      x$size <- NA
      x$instances <- NA
    }
    x$deployment <- NULL

    # this may be provided by the server at some point, but for now infer it
    # from the account type
    x$config_url <- if (isShinyapps(accountDetails))
      paste("https://www.shinyapps.io/admin/#/application", x$id, sep = "/")
    else
      sub("/__api__", paste("/connect/#/apps", x$id, sep = "/"),
          serverDetails$url)

    return(x)
  })

  # convert to data frame
  res <- do.call(rbind, res)

  as.data.frame(res, stringsAsFactors = FALSE)
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

#' Show Application Logs
#'
#' Show the logs for a deployed ShinyApps application.
#'
#' @param appPath The path to the directory or file that was deployed.
#' @param appFile The path to the R source file that contains the application
#'   (for single file applications).
#' @param appName The name of the application to show logs for. May be omitted
#'   if only one application deployment was made from \code{appPath}.
#' @param account The account under which the application was deployed. May be
#'   omitted if only one account is registered on the system.
#' @param entries The number of log entries to show. Defaults to 50 entries.
#' @param streaming Whether to stream the logs. If \code{TRUE}, then the
#'   function does not return; instead, log entries are written to the console
#'   as they are made, until R is interrupted. Defaults to \code{FALSE}.
#'
#' @note This function works only for ShinyApps servers.
#'
#' @export
showLogs <- function(appPath = getwd(), appFile = NULL, appName = NULL,
                     account = NULL, entries = 50, streaming = FALSE) {

  # determine the log target and target account info
  target <- deploymentTarget(appPath, appName, account)
  accountDetails <- accountInfo(target$account)
  client <- lucidClient(shinyappsServerInfo()$url, accountDetails)
  application <- getAppByName(client, accountDetails, target$appName)
  if (is.null(application))
    stop("No application found. Specify the application's directory, name, ",
         "and/or associated account.")

  # check for streaming log compatibility
  if (streaming) {
    httpType <- getOption("rsconnect.http", "rcurl")
    if (!identical("rcurl", httpType)) {
      stop("RCurl is required to show streaming logs. Install RCurl and set ",
            "rsconnect.http to 'rcurl', or call showLogs with streaming = ",
            "FALSE to show a log snapshot.")
    }
  }
  if (streaming) {
    # if streaming, fork a new R process to do the blocking operation
    rPath <- file.path(R.home("bin"), "R")
    killfile <- tempfile()
    outfile <- tempfile()

    # remove the output filewhen done. don't remove the killfile; wait for the
    # forked process to do that when it's finished (happens async after we
    # exit)
    on.exit(unlink(outfile), add = TRUE)

    # form the command. we need to double-escape backslashes in file names, as
    # they will get collapsed twice before being resolved.
    cmd <- paste0("rsconnect:::showStreamingLogs(",
                  "account = '", target$account, "', ",
                  "applicationId = '", application$id, "', ",
                  "entries = ", entries, ", ",
                  "outfile = '", gsub("\\", "\\\\", outfile, fixed = TRUE), "', ",
                  "killfile = '", gsub("\\", "\\\\", killfile, fixed = TRUE), "')")
    args <- paste("--vanilla", "--slave", paste0("-e \"", cmd, "\""))

    # execute the command, then wait for the file to which output will be
    # written to exist
    system2(command = rPath, args = args, stdout = NULL, stderr = NULL,
            wait = FALSE)
    tries <- 0
    repeat {
      tryCatch({
        Sys.sleep(0.1)
        tries <- tries + 1
        logReader <- file(outfile, open = "rt", blocking = FALSE)
        break
      },
      error = function(e, ...) {
        # if we can't open the file, keep trying until we can, or until we've
        # exhausted our maximum retries (~5s).
        if (tries > 500) {
          stop("Failed to start log listener.")
        }
      },
      warning = function (...) {})
    }

    # once the file is open, close it when finished
    on.exit(close(logReader), add = TRUE)

    # read from the output file until interrupted
    repeat {
      tryCatch({
        if (file.exists(killfile))
          break
        writeLines(readLines(con = logReader, warn = FALSE))
        Sys.sleep(0.1)
      },
      interrupt = function(...) {
        # when R is interrupted, write out the killfile so the async process
        # won't hang around listening for logs
        close(file(killfile, open="wt"))
      },
      error = function(e, ...) {
        print(e$message)
      })
    }
  } else {
    # if not streaming, poll for the entries directly
    logs <- client$getLogs(application$id, entries, FALSE, NULL)
    cat(logs)
  }
}

# blocks for network traffic--should be run in a child process
showStreamingLogs <- function(account, applicationId, entries, outfile,
                              killfile) {
  # get account information
  accountInfo <- accountInfo(account)
  client <- clientForAccount(accountInfo)

  # remove the killfile when we're done
  on.exit(unlink(killfile), add = TRUE)

  conn <- file(outfile, open = "wt", blocking = FALSE)

  # the server may time out the request after a few minutes--keep asking for it
  # until interrupted by the presence of the killfile
  skip <- 0
  repeat {
  tryCatch({
             client$getLogs(applicationId, entries, TRUE,
                           writeLogMessage(conn, killfile, skip))
             if (file.exists(killfile))
               break
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
}

writeLogMessage <- function(conn, killfile, skip) {
  update = function(data) {
    # write incoming log data to the console
    if (skip > 0) {
      skip <<- skip - 1
    } else {
      cat(data, file = conn)
    }
    nchar(data, "bytes")
  }
  value = function() {
    # log data is written to the console, but not returned
    return("")
  }
  reset = function() {}
  # dummy progress meter--exists to allow us to cancel requests when R is
  # interrupted
  progress <- function(down, up) {
    tryCatch((function() {
      # leave event loop for a moment to give interrupt a chance to arrive
      Sys.sleep(0.01); cat("")
      # check for the existance of the killfile--if it exists, abort the process
      if (file.exists(killfile))
        1L
      else
        0L
    })(),
    # when an error or interrupt occurs, write the killfile and tell RCurl to
    # abort the request
    error = function(e, ...) {
      close(file(killfile, open="wt"))
      1L
    },
    interrupt = function(...) {
      close(file(killfile, open="wt"))
      1L
    }
    )
  }
  writer = list(update = update, value = value, reset = reset,
                progress = progress)
  class(writer) <- c("RCurlTextHandler", "RCurlCallbackFunction")
  return(writer)
}

