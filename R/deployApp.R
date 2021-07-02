#' Deploy an Application
#'
#' Deploy a [shiny][shiny::shiny-package] application, an
#' [RMarkdown][rmarkdown::rmarkdown-package] document, a plumber API, or HTML
#' content to a server.
#'
#' In order to ignore specific files in the working directory while deploying,
#' the files must be listed in the .rscignore file. This file must have one
#' file or directory per line with no support for wildcards.'
#'
#' @param appDir Directory containing application. Defaults to current working
#'   directory.
#' @param appFiles The files and directories to bundle and deploy (only if
#'   `upload = TRUE`). Can be `NULL`, in which case all the files in the
#'   directory containing the application are bundled. Takes precedence over
#'   `appFileManifest` if both are supplied.
#' @param appFileManifest An alternate way to specify the files to be deployed;
#'   a file containing the names of the files, one per line, relative to the
#'   `appDir`.
#' @param appPrimaryDoc If the application contains more than one document, this
#'   parameter indicates the primary one, as a path relative to `appDir`. Can be
#'   `NULL`, in which case the primary document is inferred from the contents
#'   being deployed.
#' @param appSourceDoc If the application is composed of static files (e.g
#'   HTML), this parameter indicates the source document, if any, as a fully
#'   qualified path. Deployment information returned by [deployments()] is
#'   associated with the source document.
#' @param appName Name of application (names must be unique within an account).
#'   Defaults to the base name of the specified `appDir`.
#' @param appTitle Free-form descriptive title of application. Optional; if
#'   supplied, will often be displayed in favor of the name. When deploying a
#'   new application, you may supply only the `appTitle` to receive an
#'   auto-generated `appName`.
#' @param appId If updating an application, the ID of the application being
#'   updated. Optional unless updating an app owned by another user.
#' @param contentCategory Optional; the kind of content being deployed (e.g.
#'   `"plot"` or `"site"`).
#' @param account Account to deploy application to. This parameter is only
#'   required for the initial deployment of an application when there are
#'   multiple accounts configured on the system (see [accounts]).
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @param upload If `TRUE` (the default) then the application is uploaded from
#'   the local system prior to deployment. If `FALSE` then it is re-deployed
#'   using the last version that was uploaded. `FALSE` is only supported on
#'   shinyapps.io; `TRUE` is required on RStudio Connect.
#' @param recordDir Directory where publish record is written. Can be `NULL`
#'   in which case record will be written to the location specified with `appDir`.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to `TRUE` in
#'   interactive sessions only. If a function is passed, it will be called
#'   after the app is started, with the app URL as a paramter.
#' @param on.failure Function to be called if the deployment fails. If a
#'   deployment log URL is available, it's passed as a parameter.
#' @param logLevel One of `"quiet"`, `"normal"` or `"verbose"`; indicates how
#'   much logging to the console is to be performed. At `"quiet"` reports no
#'   information; at `"verbose"`, a full diagnostic log is captured.
#' @param lint Lint the project before initiating deployment, to identify
#'   potentially problematic code?
#' @param metadata Additional metadata fields to save with the deployment
#'   record. These fields will be returned on subsequent calls to
#'   [deployments()].
#' @param forceUpdate If `TRUE`, update any previously-deployed app without
#'   asking. If `FALSE`, ask to update. If unset, defaults to the value of
#'   `getOption("rsconnect.force.update.apps", FALSE)`.
#' @param python Full path to a python binary for use by `reticulate`.
#'   Required if `reticulate` is a dependency of the app being deployed.
#'   If python = NULL, and RETICULATE_PYTHON is set in the environment, its
#'   value will be used. The specified python binary will be invoked to determine
#'   its version and to list the python packages installed in the environment.
#' @param forceGeneratePythonEnvironment Optional. If an existing
#'   `requirements.txt` file is found, it will be overwritten when this argument
#'   is `TRUE`.
#' @examples
#' \dontrun{
#'
#' # deploy the application in the current working dir
#' deployApp()
#'
#' # deploy an application in another directory
#' deployApp("~/projects/shiny/app1")
#'
#' # deploy using an alternative application name and title
#' deployApp("~/projects/shiny/app1", appName = "myapp",
#'           appTitle = "My Application")
#'
#' # deploy specifying an explicit account name, then
#' # redeploy with no arguments (will automatically use
#' # the previously specified account)
#' deployApp(account = "jsmith")
#' deployApp()
#'
#' # deploy but don't launch a browser when completed
#' deployApp(launch.browser = FALSE)
#' }
#' @seealso [applications()], [terminateApp()], and [restartApp()]
#' @family Deployment functions
#' @export
deployApp <- function(appDir = getwd(),
                      appFiles = NULL,
                      appFileManifest = NULL,
                      appPrimaryDoc = NULL,
                      appSourceDoc = NULL,
                      appName = NULL,
                      appTitle = NULL,
                      appId = NULL,
                      contentCategory = NULL,
                      account = NULL,
                      server = NULL,
                      upload = TRUE,
                      recordDir = NULL,
                      launch.browser = getOption("rsconnect.launch.browser",
                                                 interactive()),
                      logLevel = c("normal", "quiet", "verbose"),
                      lint = TRUE,
                      metadata = list(),
                      forceUpdate = getOption("rsconnect.force.update.apps", FALSE),
                      python = NULL,
                      on.failure = NULL,
                      forceGeneratePythonEnvironment = FALSE) {

  condaMode <- FALSE

  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))

  # respect log level
  logLevel <- match.arg(logLevel)
  quiet <- identical(logLevel, "quiet")
  verbose <- identical(logLevel, "verbose")

  # run startup scripts to pick up any user options and establish pre/post deploy hooks
  runStartupScripts(appDir, logLevel)

  # at verbose log level, turn on all tracing options implicitly for the
  # duration of the call
  if (verbose) {
    options <- c("rsconnect.http.trace",
                 "rsconnect.http.trace.json",
                 "rsconnect.error.trace")
    restorelist <- list()
    newlist <- list()

    # record options at non-default position
    for (option in options) {
      if (!isTRUE(getOption(option))) {
        restorelist[[option]] <- FALSE
        newlist[[option]] <- TRUE
      }
    }

    # apply new option values
    options(newlist)

    # restore all old option values on exit
    on.exit(options(restorelist), add = TRUE)
  }

  # install error handler if requested
  if (isTRUE(getOption("rsconnect.error.trace"))) {
    errOption <- getOption("error")
    options(error = function(e) {
      cat("----- Deployment error -----\n")
      cat(geterrmessage(), "\n")
      cat("----- Error stack trace -----\n")
      traceback(3, sys.calls())
    })
    on.exit(options(error = errOption), add = TRUE)
  }

  # normalize appDir path and ensure it exists
  appDir <- normalizePath(appDir, mustWork = FALSE)
  if (!file.exists(appDir)) {
    stop(appDir, " does not exist")
  }

  # create the full path that we'll deploy (append document if requested)
  appPath <- appDir
  if (!is.null(appSourceDoc) && nchar(appSourceDoc) > 0) {
    appPath <- appSourceDoc
  } else if (!is.null(appPrimaryDoc) && nchar(appPrimaryDoc) > 0) {
    appPath <- file.path(appPath, appPrimaryDoc)
    if (!file.exists(appPath)) {
      stop(appPath, " does not exist")
    }
  }

  # if a specific file is named, make sure it's an Rmd or HTML, and just deploy
  # a single document in this case (this will call back to deployApp with a list
  # of supporting documents)
  rmdFile <- ""
  if (!file.info(appDir)$isdir) {
    if (grepl("\\.Rmd$", appDir, ignore.case = TRUE) ||
        grepl("\\.html?$", appDir, ignore.case = TRUE)) {
      return(deployDoc(appDir, appName = appName, appTitle = appTitle,
                       account = account, server = server, upload = upload,
                       recordDir = recordDir, launch.browser = launch.browser,
                       logLevel = logLevel, lint = lint))
    } else {
      stop(appDir, " must be a directory, an R Markdown document, or an HTML ",
           "document.")
    }
  }

  # directory for recording deployment
  if (is.null(recordDir)) {
    recordDir <- appPath
  } else {
    if (!file.exists(recordDir)) {
      stop(recordDir, " does not exist")
    }
    if (!file.info(recordDir)$isdir) {
      stop(recordDir, " must be a directory")
    }
  }

  # at verbose log level, generate header
  if (verbose) {
    cat("----- Deployment log started at ", as.character(Sys.time()), " -----\n")
    cat("Deploy command:", "\n", deparse(sys.call(1)), "\n\n")
    cat("Session information: \n")
    print(utils::sessionInfo())
  }

  # invoke pre-deploy hook if we have one
  preDeploy <- getOption("rsconnect.pre.deploy")
  if (is.function(preDeploy)) {
    if (verbose) {
      cat("Invoking pre-deploy hook rsconnect.pre.deploy\n")
    }
    preDeploy(appPath)
  }

  # figure out what kind of thing we're deploying
  if (!is.null(contentCategory)) {
    assetTypeName <- contentCategory
  } else if (!is.null(appPrimaryDoc)) {
    assetTypeName <- "document"
  } else {
    assetTypeName <- "application"
  }

  # build the list of files to deploy -- implicitly (directory contents),
  # explicitly via list, or explicitly via manifest
  if (is.null(appFiles)) {
    if (is.null(appFileManifest)) {
      # no files supplied at all, just bundle the whole directory
      appFiles <- bundleFiles(appDir)
    } else {
      # manifest file provided, read it and apply
      if (!isStringParam(appFileManifest))
        stop(stringParamErrorMessage("appFileManifest"))
      if (!file.exists(appFileManifest))
        stop(appFileManifest, " was specified as a file manifest, but does ",
             "not exist.")

      # read the filenames from the file
      manifestLines <- readLines(appFileManifest, warn = FALSE)

      # remove empty/comment lines and explode remaining
      manifestLines <- manifestLines[nzchar(manifestLines)]
      manifestLines <- manifestLines[!grepl("^#", manifestLines)]
      appFiles <- explodeFiles(appDir, manifestLines)
    }
  } else {
    # file list provided directly
    appFiles <- explodeFiles(appDir, appFiles)
  }

  if (isTRUE(lint)) {
    lintResults <- lint(appDir, appFiles, appPrimaryDoc)

    if (hasLint(lintResults)) {

      if (interactive()) {
        # if enabled, show warnings in friendly marker tab in addition to
        # printing to console
        if (getOption("rsconnect.rstudio_source_markers", TRUE) &&
            rstudioapi::hasFun("sourceMarkers"))
        {
          showRstudioSourceMarkers(appDir, lintResults)
        }
        message("The following potential problems were identified in the project files:\n")
        printLinterResults(lintResults)
        response <- readline("Do you want to proceed with deployment? [Y/n]: ")
        if (tolower(substring(response, 1, 1)) != "y") {
          message("Cancelling deployment.")
          return(invisible(lintResults))
        }
      } else {
        message("The linter has identified potential problems in the project:\n")
        printLinterResults(lintResults)
#         message(
#           "\nIf you believe these errors are spurious, run:\n\n",
#           "\tdeployApp(lint = FALSE)\n\n",
#           "to disable linting."
#         )
        message("If your ", assetTypeName, " fails to run post-deployment, ",
                "please double-check these messages.")
      }

    }

  }

  if (!is.null(appName) && !isStringParam(appName))
    stop(stringParamErrorMessage("appName"))

  # try to detect encoding from the RStudio project file
  .globals$encoding <- rstudioEncoding(appDir)
  on.exit(.globals$encoding <- NULL, add = TRUE)

  # functions to show status (respects quiet param)
  displayStatus <- displayStatus(quiet)
  withStatus <- withStatus(quiet)

  # initialize connect client

  # determine the deployment target and target account info
  target <- deploymentTarget(appPath, appName, appTitle, appId, account, server)
  accountDetails <- accountInfo(target$account, target$server)

  # test for compatibility between account type and publish intent
  if (isShinyapps(accountDetails$server)) {
    # ensure we aren't trying to publish an API to shinyapps.io; this will not
    # currently end well
    if (identical(contentCategory, "api")) {
      stop("Plumber APIs are not currently supported on shinyapps.io; they ",
           "can only be published to RStudio Connect.")
    }
  } else {
    if (identical(upload, FALSE)) {
      # it is not possible to deploy to Connect without uploading
      stop("RStudio Connect does not support deploying without uploading. ",
           "Specify upload=TRUE to upload and re-deploy your application.")
    }
  }

  client <- clientForAccount(accountDetails)
  if(verbose){
    urlstr <- serverInfo(accountDetails$server)$url
    url <- parseHttpUrl(urlstr)
    cat("Cookies:", "\n")
    host <- getCookieHost(url)
    if (exists(host, .cookieStore)){
      print(get(host, envir=.cookieStore))
    } else {
      print("None")
    }
  }

  # get the application to deploy (creates a new app on demand)
  withStatus(paste0("Preparing to deploy ", assetTypeName), {
    application <- applicationForTarget(client, accountDetails, target, forceUpdate)
  })

  if (upload) {
    # create, and upload the bundle
    if (verbose)
      cat("----- Bundle upload started at ", as.character(Sys.time()), " -----\n")
    withStatus(paste0("Uploading bundle for ", assetTypeName, ": ",
                     application$id), {

      # python is enabled on Connect but not on Shinyapps
      python <- getPythonForTarget(python, accountDetails)
      bundlePath <- bundleApp(target$appName, appDir, appFiles,
                              appPrimaryDoc, assetTypeName, contentCategory, verbose, python,
                              condaMode, forceGeneratePythonEnvironment)

      if (isShinyapps(accountDetails$server)) {

        # Step 1. Create presigned URL and register pending bundle.
        bundleSize <- file.info(bundlePath)$size

        # Generate a hex-encoded md5 hash.
        checksum <- fileMD5.as.string(bundlePath)
        bundle <- client$createBundle(application$id, "application/x-tar", bundleSize, checksum)

        if (verbose)
          timestampedLog("Starting upload now")
        # Step 2. Upload Bundle to presigned URL
        if (!uploadBundle(bundle, bundleSize, bundlePath)) {
          stop("Could not upload file.")
        }
        if (verbose)
          timestampedLog("Upload complete")

        # Step 3. Upload revise bundle status.
        response <- client$updateBundleStatus(bundle$id, status="ready")

        # Step 4. Retrieve updated bundle post status change - which is required in subsequent
        # areas of the code below.
        bundle <- client$getBundle(bundle$id)

      } else {
        bundle <- client$uploadApplication(application$id, bundlePath)
      }
    })
  } else {
    # redeploy current bundle
    bundle <- application$deployment$bundle
  }

  # write a deployment record only if this is the account that owns the content
  if (is.null(application$owner_username) ||
      accountDetails$username == application$owner_username) {
    # save the deployment info for subsequent updates--we do this before
    # attempting the deployment itself to make retry easy on failure.
    if (verbose)
      timestampedLog("Saving deployment record for", target$appName, "-", target$username)
    saveDeployment(recordDir,
                   target$appName,
                   target$appTitle,
                   target$username,
                   target$account,
                   accountDetails$server,
                   serverInfo(target$server)$hostUrl,
                   application$id,
                   bundle$id,
                   application$url,
                   metadata)
  } else if (verbose) {
    timestampedLog("Updating", target$appName, ", owned by", application$owner_username,
        ", from account", accountDetails$username)
  }

  if (length(bundle$id) > 0 && nzchar(bundle$id)) {
    displayStatus(paste0("Deploying bundle: ", bundle$id,
                         " for ", assetTypeName, ": ", application$id,
                         " ...\n", sep=""))
  }
  if (verbose) {
    cat("----- Server deployment started at ", as.character(Sys.time()), " -----\n")
  }

  # wait for the deployment to complete (will raise an error if it can't)
  task <- client$deployApplication(application$id, bundle$id)
  taskId <- if (is.null(task$task_id)) task$id else task$task_id
  response <- client$waitForTask(taskId, quiet)

  # wait 1/10th of a second for any queued output get picked by RStudio
  # before emitting the final status, to ensure it's the last line the user sees
  Sys.sleep(0.10)

  deploymentSucceeded <- if (is.null(response$code) || response$code == 0) {
    displayStatus(paste0(capitalize(assetTypeName), " successfully deployed ",
                         "to ", application$url, "\n"))
    TRUE
  } else {
    displayStatus(paste0(capitalize(assetTypeName), " deployment failed ",
                         "with error: ", response$error, "\n"))
    FALSE
  }

  if (!quiet)
    openURL(client, application, launch.browser, on.failure, deploymentSucceeded)

  # invoke post-deploy hook if we have one
  if (deploymentSucceeded) {
    postDeploy <- getOption("rsconnect.post.deploy")
    if (is.function(postDeploy)) {
      if (verbose) {
        cat("Invoking post-deploy hook rsconnect.post.deploy\n")
      }
      postDeploy(appPath)
    }
  }

  if (verbose) {
    cat("----- Deployment log finished at ", as.character(Sys.time()), " -----\n")
  }

  invisible(deploymentSucceeded)
}


getPython <- function(path) {
  if (is.null(path)) {
    path <- Sys.getenv("RETICULATE_PYTHON")
    if (path == "") {
      return(NULL)
    }
  }
  path.expand(path)
}


getPythonForTarget <- function(path, accountDetails) {
  # python is enabled on Connect but not on Shinyapps
  targetIsShinyapps <- isShinyapps(accountDetails$server)
  pythonEnabled = getOption("rsconnect.python.enabled", default=!targetIsShinyapps)
  if (pythonEnabled) {
    getPython(path)
  }
  else {
    NULL
  }
}

# calculate the deployment target based on the passed parameters and
# any saved deployments that we have
deploymentTarget <- function(appPath, appName, appTitle, appId, account,
                             server = NULL) {

  # read existing accounts
  accounts <- accounts(server)[,"name"]
  if (length(accounts) == 0)
    stopWithNoAccount()

  # validate account if provided
  if (!is.null(account)) {
    if (!account %in% accounts)
      stop(paste("Unknown account name '", account, "' (you can use the ",
                 "setAccountInfo function to add a new account)", sep = ""),
           call. = FALSE)
  }

  # read existing deployments
  appDeployments <- deployments(appPath = appPath)

  # function to compute the target username from a deployment
  usernameFromDeployment <- function(deployment) {
    # determine which username on the serve owns the application
    if (!is.null(appDeployments$username)) {
      # read from the deployment record if supplied
      username <- appDeployments$username
    } else {
      # lookup account info if not
      username <- accountInfo(appDeployments$account)$username
    }
    username
  }

  # function to create a deployment target list (checks whether the target
  # is an update and adds that field)
  createDeploymentTarget <- function(appName, appTitle, appId,
                                     username, account, server) {
    # look up the server URL
    serverDetails <- serverInfo(server)

    # look for an application ID if we weren't supplied one
    if (is.null(appId))
    {
      existingDeployments <- deployments(appPath, nameFilter = appName)
      for (i in seq_len(nrow(existingDeployments))) {
        if (identical(existingDeployments[[i, "account"]], account) &&
            identical(existingDeployments[[i, "server"]], server))
        {
          # account and server matches a locally configured account
          appId <- existingDeployments[[i, "appId"]]
          break
        }
        else if (identical(existingDeployments[[i, "username"]], username) &&
                 identical(existingDeployments[[i, "host"]], serverDetails$url))
        {
          # username and host match the user and host we're deploying to
          appId <- existingDeployments[[i, "appId"]]
          break
        }
      }
    }

    list(appName = appName, appTitle = appTitle, appId = appId, username = username,
         account = account, server = server)
  }

  # if appTitle specified but not appName, generate name from title
  if (is.null(appName) && !is.null(appTitle) && nzchar(appTitle)) {
    appName <- generateAppName(appTitle, appPath, account, unique = FALSE)
  }

  # both appName and account explicitly specified
  if (!is.null(appName) && !is.null(account)) {
    accountDetails <- accountInfo(account, server)
    createDeploymentTarget(appName, appTitle, appId, accountDetails$username,
                           account, accountDetails$server)
  }

  # just appName specified
  else if (!is.null(appName)) {

    # find any existing deployments of this application
    appDeployments <- deployments(appPath,
                                  nameFilter = appName)

    # if there are none then we can create it if there is a single account
    # registered that we can default to
    if (nrow(appDeployments) == 0) {
      if (length(accounts) == 1) {
        # read the server associated with the account
        accountDetails <- accountInfo(accounts, server)
        createDeploymentTarget(appName, appTitle, appId, accountDetails$username, accounts,
                               accountDetails$server)
      } else {
        stopWithSpecifyAccount()
      }
    }

    # single existing deployment
    else if (nrow(appDeployments) == 1) {
      createDeploymentTarget(appName, appTitle, appId,
                             usernameFromDeployment(appDeployments), appDeployments$account,
                             appDeployments$server)
    }

    # multiple existing deployments
    else if (nrow(appDeployments) > 1) {
      stop(paste("Please specify the account you want to deploy '", appName,
                 "' to (you have previously deployed this application to ",
                 "more than one account).", sep = ""), call. = FALSE)
    }

  }

  # just account/server specified, that's fine we just default the app name
  # based on the basename of the application directory
  else if (!is.null(account) || !is.null(server)) {
    if (is.null(account)) {
      account <- accounts(server)[,"name"]
      if (length(account) > 1) {
        stopWithSpecifyAccount()
      }
    }
    accountDetails <- accountInfo(account, server)
    createDeploymentTarget(
      generateAppName(appTitle, appPath, account, unique = FALSE),
      appTitle, appId, accountDetails$username, account, accountDetails$server)
  }

  # neither specified but a single existing deployment
  else if (nrow(appDeployments) == 1) {
    createDeploymentTarget(appDeployments$name,
                           appDeployments$title,
                           appDeployments$appId,
                           usernameFromDeployment(appDeployments),
                           appDeployments$account,
                           appDeployments$server)
  }

  # neither specified and no existing deployments
  else if (nrow(appDeployments) == 0) {

    # single account we can default to
    if (length(accounts) == 1) {
      accountDetails <- accountInfo(accounts)
      createDeploymentTarget(
        generateAppName(appTitle, appPath, account, unique = FALSE),
        appTitle, appId, accountDetails$username,
        accounts, accountDetails$server)
    }
    else
      stop("Please specify the account and server to which you want to deploy ",
           "the application (there is more than one account registered ",
           "on this system).", call. = FALSE)

  }

  # neither specified and multiple existing deployments
  else {

    stop("Unable to deploy using default arguments (multiple existing ",
         "deployments from this application directory already exist). ",
         "Please specify appName and/or account name explicitly.",
         call. = FALSE)

  }
}

# get the record for the application of the given name in the given account, or
# NULL if no application exists by that name
getAppByName <- function(client, accountInfo, name) {
  # NOTE: returns a list with 0 or 1 elements
  app <- client$listApplications(accountInfo$accountId, filters = list(name = name))
  if (length(app)) app[[1]] else NULL
}

# get the record for the application with the given ID in the given account;
# this isn't used inside the package itself but is invoked from the RStudio IDE
# to look up app details
getAppById <- function(id, account = NULL, server = NULL, hostUrl = NULL) {
  accountDetails <- NULL
  tryCatch({
    # attempt to look up the account locally
    accountDetails <- accountInfo(resolveAccount(account, server), server)
  }, error = function(e) {
    # we'll retry below
  })

  if (is.null(accountDetails)) {
    if (is.null(hostUrl)) {
      # rethrow if no host url to go on
      stop("No account '", account, "' found and no host URL specified.",
           call. = FALSE)
    }

    # no account details yet, look up from the host URL if we have one
    accountDetails <- accountInfoFromHostUrl(hostUrl)
  }

  # create the appropriate client and fetch the application
  client <- clientForAccount(accountDetails)
  client$getApplication(id)
}

applicationForTarget <- function(client, accountInfo, target, forceUpdate) {

  if (is.null(target$appId)) {
    # list the existing applications for this account and see if we
    # need to create a new application
    app <- getAppByName(client, accountInfo, target$appName)
  } else {
    # we already know the app's id, so just retrieve the rest of the metadata
    app <- client$getApplication(target$appId)
  }

  # if there is no record of deploying this application locally however there
  # is an application of that name already deployed then confirm
  if (!is.null(target$appId) && !is.null(app) && interactive() && !forceUpdate) {
    prompt <- paste("Update application currently deployed at\n", app$url,
                    "? [Y/n] ", sep="")
    input <- readline(prompt)
    if (nzchar(input) && !identical(input, "y") && !identical(input, "Y"))
      stop("Application deployment cancelled", call. = FALSE)
  }

  # create the application if we need to
  if (is.null(app)) {
    app <- client$createApplication(target$appName, target$appTitle, "shiny",
                                    accountInfo$accountId)
  }

  # return the application
  app
}

validURL <- function(url) {
  !(is.null(url) || url == '')
}

openURL <- function(client, application, launch.browser, on.failure, deploymentSucceeded) {

  # function to browse to a URL using user-supplied browser (config or final)
  showURL <- function(url) {
    if (isTRUE(launch.browser))
      utils::browseURL(url)
    else if (is.function(launch.browser))
      launch.browser(url)
  }

  # Check to see if we should open config url or app url
  if (!is.null(client$configureApplication)) {
    config <- client$configureApplication(application$id)
    url <- config$config_url
    if (!deploymentSucceeded && validURL(config$logs_url)) {
      # With 1.5.5+, Connect application configuration includes
      # a logs URL to be shown on unsuccessful deployment.
      url <- config$logs_url
    }
    if (validURL(url)) {
      # Connect should always end up here, even on deployment failures
      if (deploymentSucceeded) {
        showURL(url)
      } else if (is.function(on.failure)) {
        on.failure(url)
      }
    }
  } else if (deploymentSucceeded) {
    # shinyapps.io should land here if things succeeded
    showURL(application$url)
  } else if (is.function(on.failure)) {
    on.failure(NULL)
  }
    # or open no url if things failed
}

runStartupScripts <- function(appDir, logLevel) {
  scripts <- c(
    # the site-wide startup script
    file.path(R.home("etc"), "rsconnect.site"),
    # the user startup script
    path.expand("~/.rsconnect_profile"),
    # a startup script specific to this application
    file.path(appDir, ".rsconnect_profile"))

  # iterate over the startup scripts
  for (script in scripts) {
    if (file.exists(script)) {
      if (logLevel == "verbose") {
        cat("----- Sourcing startup script", script, "-----\n")
      }
      source(file = script, verbose = (logLevel == "verbose"))
    }
  }
}
