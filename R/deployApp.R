#' Deploy an Application
#'
#' Deploy a \link[shiny:shiny-package]{shiny} application, an R Markdown
#' document, or HTML content to a server.
#'
#' @param appDir Directory containing application. Defaults to current working
#'   directory.
#' @param appFiles The files and directories to bundle and deploy (only if
#'   \code{upload = TRUE}). Can be \code{NULL}, in which case all the files in
#'   the directory containing the application are bundled. Takes precedence over
#'   \code{appFileManifest} if both are supplied.
#' @param appFileManifest An alternate way to specify the files to be deployed;
#'   a file containing the names of the files, one per line, relative to the
#'   \code{appDir}.
#' @param appPrimaryDoc If the application contains more than one document, this
#'   parameter indicates the primary one, as a path relative to \code{appDir}.
#'   Can be \code{NULL}, in which case the primary document is inferred from the
#'   contents being deployed.
#' @param appSourceDoc If the application is composed of static files (e.g
#'   HTML), this parameter indicates the source document, if any, as a fully
#'   qualified path. Deployment information returned by
#'   \code{\link{deployments}} is associated with the source document.
#' @param appName Name of application (names must be unique within an
#'   account). Defaults to the base name of the specified \code{appDir}.
#' @param contentCategory Optional; the kind of content being deployed (e.g.
#'   \code{"plot"}, \code{"document"}, or \code{"application"}).
#' @param account Account to deploy application to. This
#'   parameter is only required for the initial deployment of an application
#'   when there are multiple accounts configured on the system (see
#'   \link{accounts}).
#' @param server Server name. Required only if you use the same account name on
#'   multiple servers.
#' @param upload If \code{TRUE} (the default) then the application is uploaded
#'   from the local system prior to deployment. If \code{FALSE} then it is
#'   re-deployed using the last version that was uploaded.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to \code{TRUE} in
#'   interactive sessions only.
#' @param quiet Request that no status information be printed to the console
#'   during the deployment.
#' @param lint Lint the project before initiating deployment, to identify
#'   potentially problematic code?
#' @param metadata Additional metadata fields to save with the deployment
#'   record. These fields will be returned on subsequent calls to
#'   \code{\link{deployments}}.
#' @examples
#' \dontrun{
#'
#' # deploy the application in the current working dir
#' deployApp()
#'
#' # deploy an application in another directory
#' deployApp("~/projects/shiny/app1")
#'
#' # deploy using an alternative application name
#' deployApp("~/projects/shiny/app1", appName = "myapp")
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
#' @seealso \code{\link{applications}}, \code{\link{terminateApp}}, and
#'   \code{\link{restartApp}}
#' @export
deployApp <- function(appDir = getwd(),
                      appFiles = NULL,
                      appFileManifest = NULL,
                      appPrimaryDoc = NULL,
                      appSourceDoc = NULL,
                      appName = NULL,
                      contentCategory = NULL,
                      account = NULL,
                      server = NULL,
                      upload = TRUE,
                      launch.browser = getOption("rsconnect.launch.browser",
                                                 interactive()),
                      quiet = FALSE,
                      lint = TRUE,
                      metadata = list()) {

  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))

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
      return(deployDoc(appDir, appName = appName, account = account,
                       server = server, upload = upload,
                       launch.browser = launch.browser, quiet = quiet,
                       lint = lint))
    } else {
      stop(appDir, " must be a directory, an R Markdown document, or an HTML ",
           "document.")
    }
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
        stop(appFileManifest, " was specified as a file manifest, but does not",
             "exist.")

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
  target <- deploymentTarget(appPath, appName, account, server)
  accountDetails <- accountInfo(target$account, target$server)
  client <- clientForAccount(accountDetails)

  # get the application to deploy (creates a new app on demand)
  withStatus(paste0("Preparing to deploy ", assetTypeName), {
    application <- applicationForTarget(client, accountDetails, target)
  })

  if (upload) {
    # create, and upload the bundle
    withStatus(paste0("Uploading bundle for ", assetTypeName, ": ",
                     application$id), {
      bundlePath <- bundleApp(target$appName, appDir, appFiles,
                              appPrimaryDoc, assetTypeName, contentCategory)
      bundle <- client$uploadApplication(application$id, bundlePath)
    })
  } else {
    # redeploy current bundle
    bundle <- application$deployment$bundle
  }

  # save the deployment info for subsequent updates--we do this before
  # attempting the deployment itself to make retry easy on failure
  saveDeployment(appPath,
                 target$appName,
                 target$account,
                 accountDetails$server,
                 application$id,
                 bundle$id,
                 application$url,
                 metadata)

  # wait for the deployment to complete (will raise an error if it can't)
  displayStatus(paste0("Deploying bundle: ", bundle$id,
                       " for ", assetTypeName, ": ", application$id,
                       " ...\n", sep=""))
  task <- client$deployApplication(application$id, bundle$id)
  taskId <- if (is.null(task$task_id)) task$id else task$task_id
  response <- client$waitForTask(taskId, quiet)

  # wait 1/10th of a second for any queued output get picked by RStudio
  # before emitting the final status, to ensure it's the last line the user sees
  Sys.sleep(0.10)

  deploymentSucceeded <- if (is.null(response$code) || response$code == 0) {
    displayStatus(paste0(capitalize(assetTypeName), " successfully deployed ",
                         "to ", application$url, "\n"))
    # function to browse to a URL using user-supplied browser (config or final)
    showURL <- function(url) {
      if (isTRUE(launch.browser))
        utils::browseURL(url)
      else if (is.function(launch.browser))
        launch.browser(url)
    }

    # if this client supports config, see if the app needs it
    if (!quiet && !is.null(client$configureApplication)) {
      config <- client$configureApplication(application$id)
      if (config$needs_config) {
        # app needs config, finish deployment on the server
        showURL(config$config_url)
        return(invisible(TRUE))
      }
    }

    # launch the browser if requested
    showURL(application$url)

    TRUE
  } else {
    displayStatus(paste0(capitalize(assetTypeName), " deployment failed ",
                         "with error: ", response$error, "\n"))
    FALSE
  }

  invisible(deploymentSucceeded)
}

# calculate the deployment target based on the passed parameters and
# any saved deployments that we have
deploymentTarget <- function(appPath, appName, account, server = NULL) {

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

  # function to create a deployment target list (checks whether the target
  # is an update and adds that field)
  createDeploymentTarget <- function(appName, account, server) {

    # check to see whether this is an update
    existingDeployment <- deployments(appPath,
                                      nameFilter = appName,
                                      accountFilter = account,
                                      serverFilter = server)
    isUpdate <- nrow(existingDeployment) == 1

    list(appName = appName, account = account, isUpdate = isUpdate,
         server = server)
  }


  # both appName and account explicitly specified
  if (!is.null(appName) && !is.null(account)) {

    createDeploymentTarget(appName, account, server)

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
        createDeploymentTarget(appName, accounts, accountDetails$server)
      } else {
        stopWithSpecifyAccount()
      }
    }

    # single existing deployment
    else if (nrow(appDeployments) == 1) {
      createDeploymentTarget(appName, appDeployments$account,
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
      tools::file_path_sans_ext(basename(appPath)),
      account, accountDetails$server)
  }

  # neither specified but a single existing deployment
  else if (nrow(appDeployments) == 1) {

    createDeploymentTarget(appDeployments$name,
                           appDeployments$account,
                           appDeployments$server)

  }

  # neither specified and no existing deployments
  else if (nrow(appDeployments) == 0) {

    # single account we can default to
    if (length(accounts) == 1) {
      accountDetails <- accountInfo(accounts)
      createDeploymentTarget(
        tools::file_path_sans_ext(basename(appPath)),
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

# get the record for the application with the given ID in the given account
getAppById <- function(id, account = NULL, server = NULL) {
  accountDetails <- accountInfo(resolveAccount(account, server), server)
  client <- clientForAccount(accountDetails)
  client$getApplication(id)
}

applicationForTarget <- function(client, accountInfo, target) {

  # list the existing applications for this account and see if we
  # need to create a new application
  app <- getAppByName(client, accountInfo, target$appName)

  # if there is no record of deploying this application locally however there
  # is an application of that name already deployed then confirm
  if (!target$isUpdate && !is.null(app) && interactive()) {
    prompt <- paste("Update application currently deployed at\n", app$url,
                    "? [Y/n] ", sep="")
    input <- readline(prompt)
    if (nzchar(input) && !identical(input, "y") && !identical(input, "Y"))
      stop("Application deployment cancelled", call. = FALSE)
  }

  # create the application if we need to
  if (is.null(app)) {
    app <- client$createApplication(target$appName, "shiny",
                                    accountInfo$accountId)
  }

  # return the application
  app
}
