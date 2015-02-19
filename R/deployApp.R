#' Deploy an Application
#'
#' Deploy a \link[shiny:shiny-package]{shiny} application
#'
#' @param appDir Directory containing application. Defaults to current working
#'   directory.
#' @param appFiles The files to bundle and deploy (only if \code{upload =
#'   TRUE}). Can be \code{NULL}, in which case all the files in the directory
#'   containing the application are bundled.
#' @param appPrimaryRmd If the application is contains one or more R Markdown
#'   documents, this parameter indicates the primary one. Can be \code{NULL}, in
#'   which case the primary document is inferred from the contents being
#'   deployed.
#' @param appName Name of application (names must be unique within an
#'   account). Defaults to the base name of the specified \code{appDir}.
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
                      appPrimaryRmd = NULL,
                      appName = NULL,
                      account = NULL,
                      server = NULL,
                      upload = TRUE,
                      launch.browser = getOption("rsconnect.launch.browser",
                                                 interactive()),
                      quiet = FALSE,
                      lint = TRUE) {

  if (!isStringParam(appDir))
    stop(stringParamErrorMessage("appDir"))

  # normalize appDir path and ensure it exists
  appDir <- normalizePath(appDir, mustWork = FALSE)
  if (!file.exists(appDir)) {
    stop(appDir, " does not exist")
  }

  # create the full path that we'll deploy (append document if requested)
  appPath <- appDir
  if (!is.null(appPrimaryRmd)) {
    appPath <- file.path(appPath, appPrimaryRmd)
    if (!file.exists(appPath)) {
      stop(appPath, " does not exist")
    }
  }

  # if a specific file is named, make sure it's an Rmd, and just deploy a single
  # document in this case
  rmdFile <- ""
  if (!file.info(appDir)$isdir) {
    if (grepl("\\.Rmd$", appDir, ignore.case = TRUE)) {
      return(deployDoc(appDir, appName = appName, account = account,
                       server = server, upload = upload,
                       launch.browser = launch.browser, quiet = quiet,
                       lint = lint))
    } else {
      stop(appDir, " must be a directory or an R Markdown document")
    }
  }

  # if the list of files wasn't specified, generate it
  if (is.null(appFiles)) {
    appFiles <- bundleFiles(appDir)
  }

  if (isTRUE(lint)) {
    lintResults <- lint(appDir, appFiles)

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
        message("If your application fails to run post-deployment, please double-check these messages.")
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
  withStatus("Preparing to deploy application", {
    application <- applicationForTarget(client, accountDetails, target)
  })

  if (upload) {
    # create, and upload the bundle
    withStatus("Uploading application bundle", {
      bundlePath <- bundleApp(target$appName, appDir, appFiles,
                              appPrimaryRmd, accountDetails)
      bundle <- client$uploadApplication(application$id, bundlePath)
    })
  } else {
    # redeploy current bundle
    bundle <- application$deployment$bundle
  }

  # wait for the deployment to complete (will raise an error if it can't)
  displayStatus(paste("Deploying application: ",
                      application$id,
                      "...\n", sep=""))
  task <- client$deployApplication(application$id, bundle$id)
  taskId <- if (is.null(task$task_id)) task$id else task$task_id
  response <- client$waitForTask(taskId, quiet)
  if (!is.null(response$code) && response$code != 0) {
    displayStatus(paste0("Application deployment failed with error: ",
                         response$error, "\n"))
    return
  } else {
    displayStatus(paste0("Application successfully deployed to ",
                        application$url, "\n"))
  }

  # save the deployment info for subsequent updates
  saveDeployment(appPath,
                 target$appName,
                 target$account,
                 accountDetails$server,
                 bundle$id,
                 application$url)

  # if this client supports config, see if the app needs it
  if (!quiet && !is.null(client$configureApplication)) {
    config <- client$configureApplication(application$id)
    if (config$needs_config) {
      # app needs config, finish deployment on the server
      utils::browseURL(config$config_url)
      return(invisible(TRUE))
    }
  }

  # append the file to be launched to the URL if necessary
  amendedUrl <- application$url

  # check for a launch file (i.e. an Rmd file)
  launchFile <- if (nzchar(rmdFile)) rmdFile else guessLaunchFile(appDir)
  if (nchar(launchFile) > 0) {
    if (substr(amendedUrl, nchar(amendedUrl), nchar(amendedUrl)) != "/")
      amendedUrl = paste(amendedUrl, "/", sep = "")
    amendedUrl = paste(amendedUrl, launchFile, sep = "")
  }

  # launch the browser if requested
  if (isTRUE(launch.browser))
    utils::browseURL(amendedUrl)
  else if (is.function(launch.browser))
    launch.browser(amendedUrl)

  # successful deployment!
  invisible(TRUE)
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
                 "more than one account).",sep = ""), call. = FALSE)
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

# given a directory, return the name of the file from the directory to launch,
# or an empty string if it's meaningful to launch the directory with no file.
guessLaunchFile <- function(appDir) {
  # no file to launch for Shiny, or for folders with an index.Rmd
  if (file.exists(file.path(appDir, "ui.R")) ||
      file.exists(file.path(appDir, "server.R")) ||
      file.exists(file.path(appDir, "index.Rmd")))
    return("")

  # otherwise, guess an Rmd to launch
  appFiles <- list.files(path = appDir, pattern = glob2rx("*.Rmd"),
                         recursive = FALSE, ignore.case = TRUE)

  if (length(appFiles) == 0)
    return("")

  # launch whichever file was most recently saved in the folder (presumably the
  # one the user has been working on)
  appFileInfo <- file.info(file.path(appDir, appFiles))
  appFiles[which.max(appFileInfo$mtime)]
}
