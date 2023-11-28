#' Deploy an Application
#'
#' Deploy a [shiny][shiny::shiny-package] application, an
#' [RMarkdown][rmarkdown::rmarkdown-package] document, a plumber API, or HTML
#' content to a server.
#'
#' ## Deployment records
#'
#' When deploying an app, `deployApp()` will save a deployment record that
#' makes it easy to update the app on server from your local source code. This
#' generally means that you need to only need to supply important arguments
#' (e.g. `appName`, `appTitle`, `server`/`account`) on the first deploy, and
#' rsconnect will reuse the same settings on subsequent deploys.
#'
#' The metadata needs to make this work is stored in `{appDir}/rsconnect/`.
#' You should generally check these files into version control to ensure that
#' future you and other collaborators will publish to the same location.
#'
#' If you have lost this directory, all is not lost, as `deployApp()` will
#' attempt to rediscover existing deployments. This is easiest if you are
#' updating an app that you created, as you can just supply the `appName`
#' (and `server`/`account` if you have multiple accounts) and `deployApp()`
#' will find the existing application account. If you need to update an app
#' that was created by someone else (that you have write permission) for, you'll
#' instead need to supply the `appId`.
#'
#' @param appDir A directory containing an application (e.g. a Shiny app
#'   or plumber API). Defaults to the current directory.
#' @param appFiles,appFileManifest Use `appFiles` to specify a
#'   character vector of files to bundle in the app or `appManifestFiles`
#'   to provide a path to a file containing a list of such files. If neither
#'   are supplied, will bundle all files in `appDir`, apart from standard
#'   exclusions and files listed in a `.rscignore` file. See
#'   [listDeploymentFiles()] for more details.
#' @param appPrimaryDoc If the application contains more than one document, this
#'   parameter indicates the primary one, as a path relative to `appDir`. Can be
#'   `NULL`, in which case the primary document is inferred from the contents
#'   being deployed.
#' @param appSourceDoc `r lifecycle::badge("deprecated")` Please use
#'   `recordDir` instead.
#' @param appName Application name, a string consisting of letters, numbers,
#'   `_` and `-`. The application name is used to identify applications on a
#'   server, so must be unique.
#'
#'   If not specified, the first deployment will be automatically it from the
#'   `appDir` for directory and website, and from the `appPrimaryDoc` for
#'   document. On subsequent deploys, it will use the previously stored value.
#' @param appTitle Free-form descriptive title of application. Optional; if
#'   supplied, will often be displayed in favor of the name. If ommitted,
#'   on second and subsequent deploys, the title will be unchanged.
#' @param envVars A character vector giving the names of environment variables
#'   whose values should be synchronised with the server (currently supported by
#'   Connect only). The values of the environment variables are sent over an
#'   encrypted connection and are not stored in the bundle, making this a safe
#'   way to send private data to Connect.
#'
#'   The names (not values) are stored in the deployment record so that future
#'   deployments will automatically update their values. Other environment
#'   variables on the server will not be affected. This means that removing an
#'   environment variable from `envVars` will leave it unchanged on the server.
#'   To remove it, either delete it using the Connect UI, or temporarily unset
#'   it (with `Sys.unsetenv()` or similar) then re-deploy.
#'
#'   Environment variables are set prior to deployment so that your code
#'   can use them and the first deployment can still succeed. Note that means
#'   that if the deployment fails, the values will still be updated.
#' @param appId Use this to deploy to an exact known application, ignoring all
#'   existing deployment records and `appName`.
#'
#'   You can use this to update an existing application that is missing a
#'   deployment record. If you're re-deploying an application that you
#'   created it's generally easier to use `appName`; `appId` is best reserved
#'   for re-deploying apps created by someone else.
#'
#'   You can find the `appId` in the following places:
#'   * On shinyapps.io, it's the `id` listed on the applications page.
#'   * For Posit Connect, it's `guid` from the info tab on the content page.
#' @param appMode Optional; the type of content being deployed.
#'   Provide this option when the inferred type of content is incorrect. This
#'   can happen, for example, when static HTML content includes a downloadable
#'   Shiny application `app.R`. Accepted values include `"shiny"`, `"api"`,
#'   `"rmd-static"`, `"rmd-shiny"`, `"quarto-static"`, `"quarto-shiny"`, and
#'   `"static"`. The Posit Connect API Reference contains a full set of
#'   available values. Not all servers support all types of content.
#' @param contentCategory Optional; classifies the kind of content being
#'   deployed (e.g. `"plot"` or `"site"`).
#' @param account,server Uniquely identify a remote server with either your
#'   user `account`, the `server` name, or both. If neither are supplied, and
#'   there are multiple options, you'll be prompted to pick one.
#'
#'   Use [accounts()] to see the full list of available options.
#' @param upload If `TRUE` (the default) then the application is uploaded from
#'   the local system prior to deployment. If `FALSE` then it is re-deployed
#'   using the last version that was uploaded. `FALSE` is only supported on
#'   shinyapps.io; `TRUE` is required on Posit Connect.
#' @param recordDir Directory where deployment record is written. The default,
#'   `NULL`, uses `appDir`, since this is usually where you want the deployment
#'   data to be stored. This argument is typically only needed when deploying
#'   a directory of static files since you want to store the record with the
#'   code that generated those files, not the files themselves.
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
#'
#'   Multi-value fields are recorded as comma-separated values and returned in
#'   that form. Custom value serialization is the responsibility of the caller.
#' @param forceUpdate What should happen if there's no deployment record for
#'   the app, but there's an app with the same name on the server? If `TRUE`,
#'   will always update the previously-deployed app. If `FALSE`, will ask
#'   the user what to do, or fail if not in an interactive context.
#'
#'   Defaults to `TRUE` when called automatically by the IDE, and `FALSE`
#'   otherwise. You can override the default by setting option
#'   `rsconnect.force.update.apps`.
#' @param python Full path to a python binary for use by `reticulate`.
#'   Required if `reticulate` is a dependency of the app being deployed.
#'   If python = NULL, and RETICULATE_PYTHON or RETICULATE_PYTHON_FALLBACK is
#'   set in the environment, its value will be used. The specified python binary
#'   will be invoked to determine its version and to list the python packages
#'   installed in the environment.
#' @param forceGeneratePythonEnvironment Optional. If an existing
#'   `requirements.txt` file is found, it will be overwritten when this argument
#'   is `TRUE`.
#' @param quarto Should the deployed content be built by quarto?
#'   (`TRUE`, `FALSE`, or `NA`). The default, `NA`, will use quarto if
#'   there are `.qmd` files in the bundle, or if there is a
#'   `_quarto.yml` and `.Rmd` files.
#'
#'   (This option is ignored and quarto will always be used if the
#'   `metadata` contains `quarto_version` and `quarto_engines` fields.)
#' @param appVisibility One of `NULL`, `"private"`, or `"public"`; the
#'   visibility of the deployment. When `NULL`, no change to visibility is
#'   made. Currently has an effect only on deployments to shinyapps.io.
#' @param image Optional. The name of the image to use when building and
#'   executing this content. If none is provided, Posit Connect will
#'   attempt to choose an image based on the content requirements.
#' @param envManagement Optional. Should Posit Connect install R and Python
#'   packages for this content? (`TRUE`, `FALSE`, or `NULL`).
#'   The default, `NULL`, will not write any values to the bundle manifest,
#'   and Connect will fall back to the application default environment
#'   management strategy, or the server default if no application default
#'   is defined.
#'
#'   (This option is a shorthand flag which overwrites the values of both
#'   `envManagementR` and `envManagementPy`.)
#' @param envManagementR Optional. Should Posit Connect install R packages
#'   for this content? (`TRUE`, `FALSE`, or `NULL`). The default, `NULL`, will
#'   not write any values to the bundle manifest, and Connect will fall back to
#'   the application default R environment management strategy, or the server
#'   default if no application default is defined.
#'
#'   (This option is ignored when `envManagement` is non-`NULL`.)
#' @param envManagementPy Optional. Should Posit Connect install Python packages
#'   for this content? (`TRUE`, `FALSE`, or `NULL`). The default, `NULL`, will
#'   not write any values to the bundle manifest, and Connect will fall back to
#'   the application default Python environment management strategy, or the
#'   server default if no application default is defined.
#'
#'   (This option is ignored when `envManagement` is non-`NULL`.)
#' @param space Optional. For Posit Cloud, the id of the space where the content
#'   should be deployed. If none is provided, content will be deployed to the
#'   deploying user's workspace or deployed to the same space in case of
#'   redeploy.
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
#'
#' # deploy a Quarto website, using the quarto package to
#' # find the Quarto binary
#' deployApp("~/projects/quarto/site1")
#'
#' # deploy application with environment variables
#' # (e.g., `SECRET_PASSWORD=XYZ` is set via an ~/.Renviron file)
#' rsconnect::deployApp(envVars = c("SECRET_PASSWORD"))
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
                      envVars = NULL,
                      appId = NULL,
                      appMode = NULL,
                      contentCategory = NULL,
                      account = NULL,
                      server = NULL,
                      upload = TRUE,
                      recordDir = NULL,
                      launch.browser = getOption("rsconnect.launch.browser",
                                                 is_interactive()),
                      on.failure = NULL,
                      logLevel = c("normal", "quiet", "verbose"),
                      lint = TRUE,
                      metadata = list(),
                      forceUpdate = NULL,
                      python = NULL,
                      forceGeneratePythonEnvironment = FALSE,
                      quarto = NA,
                      appVisibility = NULL,
                      image = NULL,
                      envManagement = NULL,
                      envManagementR = NULL,
                      envManagementPy = NULL,
                      space = NULL
                      ) {

  check_string(appDir)
  if (isStaticFile(appDir) && !dirExists(appDir)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "deployApp(appDir = 'takes a directory, not a document,')",
      with = "deployDoc()"
    )
    return(deployDoc(
      appDir,
      appName = appName,
      appTitle = appTitle,
      account = account,
      server = server,
      upload = upload,
      recordDir = recordDir,
      launch.browser = launch.browser,
      logLevel = logLevel,
      lint = lint
    ))
  }
  check_directory(appDir)
  appDir <- normalizePath(appDir)

  check_string(appName, allow_null = TRUE)

  if (!is.null(appPrimaryDoc)) {
    check_string(appPrimaryDoc)
    if (!file.exists(file.path(appDir, appPrimaryDoc))) {
      cli::cli_abort("`appPrimaryDoc` not found inside `appDir`")
    }
  }

  if (!is.null(appSourceDoc)) {
    # Used by IDE so can't deprecate
    recordDir <- appSourceDoc
  }

  # set up logging helpers
  logLevel <- match.arg(logLevel)
  quiet <- identical(logLevel, "quiet")
  verbose <- identical(logLevel, "verbose")
  logger <- verboseLogger(verbose)
  displayStatus <- displayStatus(quiet)

  # run startup scripts to pick up any user options and establish pre/post deploy hooks
  runStartupScripts(appDir, quiet = quiet, verbose = verbose)

  # at verbose log level, turn on all tracing options implicitly for the
  # duration of the call
  if (verbose) {
    old_verbose <- options(
      rsconnect.http.trace = TRUE,
      rsconnect.http.trace.json = TRUE,
      rsconnect.error.trace = TRUE
    )
    defer(options(old_verbose))
  }

  # install error handler if requested
  if (isTRUE(getOption("rsconnect.error.trace"))) {
    old_error <- options(error = function(e) {
      cat("----- Deployment error -----\n")
      cat(geterrmessage(), "\n")
      cat("----- Error stack trace -----\n")
      traceback(x = sys.calls(), max.lines = 3)
    })
    defer(options(old_error))
  }

  # at verbose log level, generate header
  if (verbose) {
    logger("Deployment log started")
    cat("Deploy command:", "\n", deparse(sys.call(1)), "\n\n")
    cat("Session information: \n")
    print(utils::sessionInfo())
  }

  # invoke pre-deploy hook if we have one
  runDeploymentHook(appDir, "rsconnect.pre.deploy", verbose = verbose)

  appFiles <- listDeploymentFiles(appDir, appFiles, appFileManifest)

  if (isTRUE(lint)) {
    lintResults <- lint(appDir, appFiles, appPrimaryDoc)
    showLintResults(appDir, lintResults)
  }

  if (!quiet) {
    cli::cli_rule("Preparing for deployment")
  }

  forceUpdate <- forceUpdate %||% getOption("rsconnect.force.update.apps") %||% fromIDE()

  # determine the target deployment record and deploying account
  recordPath <- findRecordPath(appDir, recordDir, appPrimaryDoc)
  target <- findDeploymentTarget(
    recordPath = recordPath,
    appId = appId,
    appName = appName,
    appTitle = appTitle,
    envVars = envVars,
    account = account,
    server = server,
    forceUpdate = forceUpdate
  )
  accountDetails <- target$accountDetails
  deployment <- target$deployment

  if (is.null(deployment$appId)) {
    dest <- accountLabel(accountDetails$name, accountDetails$server)
    taskComplete(quiet, "Deploying {.val {deployment$name}} using {.val {dest}}")
  } else {
    dest <- accountLabel(accountDetails$name, accountDetails$server)
    taskComplete(quiet, "Re-deploying {.val {deployment$name}} using {.val {dest}}")
  }

  # Run checks prior to first saveDeployment() to avoid errors that will always
  # prevent a successful upload from generating a partial deployment
  if (!isCloudServer(accountDetails$server) && identical(upload, FALSE)) {
    # it is not possible to deploy to Connect without uploading
    stop("Posit Connect does not support deploying without uploading. ",
         "Specify upload=TRUE to upload and re-deploy your application.")
  }
  if (!isConnectServer(accountDetails$server) && length(envVars) > 1) {
    cli::cli_abort("{.arg envVars} only supported for Posit Connect servers")
  }

  client <- clientForAccount(accountDetails)
  if (verbose) {
    showCookies(serverInfo(accountDetails$server)$url)
  }

  isShinyappsServer <- isShinyappsServer(accountDetails$server)

  logger("Inferring App mode and parameters")
  appMetadata <- appMetadata(
    appDir = appDir,
    appFiles = appFiles,
    appPrimaryDoc = appPrimaryDoc,
    quarto = quarto,
    appMode = appMode,
    contentCategory = contentCategory,
    isShinyappsServer = isShinyappsServer,
    metadata = metadata
  )

  if (is.null(deployment$appId)) {
    taskStart(quiet, "Creating application on server...")
    application <- client$createApplication(
      deployment$name,
      deployment$title,
      "shiny",
      accountDetails$accountId,
      appMetadata$appMode,
      contentCategory,
      space
    )
    taskComplete(quiet, "Created application with id {.val {application$id}}")
  } else {
    taskStart(quiet, "Looking up application with id {.val {deployment$appId}}...")
    application <- tryCatch(
      {
        application <- client$getApplication(deployment$appId, deployment$version)
        taskComplete(quiet, "Found application {.url {application$url}}")

        if (identical(application$type, "static")) {
          application$application_id <- client$createRevision(application, contentCategory)
        }

        application
      },
      rsconnect_http_404 = function(err) {
        application <- applicationDeleted(client, deployment, recordPath, appMetadata)
        taskComplete(quiet, "Created application with id {.val {application$id}}")
        application
      }
    )
  }
  saveDeployment(
    recordPath,
    deployment = deployment,
    application = application,
    metadata = metadata
  )

  # Change _visibility_ & set env vars before uploading contents
  if (needsVisibilityChange(accountDetails$server, application, appVisibility)) {
    taskStart(quiet, "Setting visibility to {appVisibility}...")
    client$setApplicationProperty(
      application$id,
      "application.visibility",
      appVisibility
    )
    taskComplete(quiet, "Visibility updated")
  }
  if (length(deployment$envVars) > 0) {
    taskStart(quiet, "Updating environment variables {envVars}...")
    client$setEnvVars(application$guid, deployment$envVars)
    taskComplete(quiet, "Environment variables updated")
  }

  if (upload) {
    python <- getPythonForTarget(python, accountDetails)
    pythonConfig <- pythonConfigurator(python, forceGeneratePythonEnvironment)

    taskStart(quiet, "Bundling {length(appFiles)} file{?s}: {.file {appFiles}}")
    bundlePath <- bundleApp(
      appName = deployment$name,
      appDir = appDir,
      appFiles = appFiles,
      appMetadata = appMetadata,
      quiet = quiet,
      verbose = verbose,
      pythonConfig = pythonConfig,
      image = image,
      envManagement = envManagement,
      envManagementR = envManagementR,
      envManagementPy = envManagementPy
    )
    size <- format(file_size(bundlePath), big.mark = ",")
    taskComplete(quiet, "Created {size}b bundle")

    # create, and upload the bundle
    taskStart(quiet, "Uploading bundle...")
    if (isCloudServer(accountDetails$server)) {
      bundle <- uploadCloudBundle(client, application$application_id, bundlePath)
    } else {
      bundle <- client$uploadApplication(application$id, bundlePath)
    }
    taskComplete(quiet, "Uploaded bundle with id {.val {bundle$id}}")

    saveDeployment(
      recordPath,
      deployment = deployment,
      application = application,
      bundleId = bundle$id,
      metadata = metadata
    )
  } else {
    # redeploy current bundle
    bundle <- application$deployment$bundle
  }

  if (!quiet) {
    cli::cli_rule("Deploying to server")
  }
  task <- client$deployApplication(application, bundle$id, space)
  taskId <- if (is.null(task$task_id)) task$id else task$task_id
  # wait for the deployment to complete (will raise an error if it can't)
  response <- client$waitForTask(taskId, quiet)
  if (!quiet) {
    cli::cli_rule("Deployment complete")
  }

  # wait 1/10th of a second for any queued output get picked by RStudio
  # before emitting the final status, to ensure it's the last line the user sees
  Sys.sleep(0.10)

  deploymentSucceeded <- is.null(response$code) || response$code == 0
  if (!quiet) {
    if (deploymentSucceeded) {
      cli::cli_alert_success("Successfully deployed to {.url {application$url}}")
    } else {
      cli::cli_alert_danger("Deployment failed with error: {response$error}")
    }
  }

  if (!quiet)
    openURL(client, application, launch.browser, on.failure, deploymentSucceeded)

  # invoke post-deploy hook if we have one
  if (deploymentSucceeded) {
    runDeploymentHook(appDir, "rsconnect.post.deploy", verbose = verbose)
  }

  logger("Deployment log finished")

  invisible(deploymentSucceeded)
}

taskStart <- function(quiet, message, .envir = caller_env()) {
  if (quiet) return()
  cli::cli_alert_info(message, .envir = .envir)
}
taskComplete <- function(quiet, message, .envir = caller_env()) {
  if (quiet) return()
  cli::cli_alert_success(message, .envir = .envir)
}

findRecordPath <- function(appDir,
                           recordDir = NULL,
                           appPrimaryDoc = NULL) {
  if (!is.null(recordDir)) {
    recordDir
  } else if (!is.null(appPrimaryDoc)) {
    file.path(appDir, appPrimaryDoc)
  } else {
    appDir
  }
}

# Need to set _before_ deploy
needsVisibilityChange <- function(server, application, appVisibility = NULL) {
  if (is.null(appVisibility)) {
    return(FALSE)
  }

  if (isConnectServer(server)) {
    # Defaults to private visibility
    return(FALSE)
  }

  if (!isShinyappsServer(server)) {
    cli::cli_abort(c(
      "Can't change cloud app visiblity from {.fun deployApp}.",
      i = "Please change on posit.cloud instead."
    ))
  }

  cur <- application$deployment$properties$application.visibility
  if (is.null(cur)) {
    cur <- "public"
  }
  cur != appVisibility
}

runDeploymentHook <- function(appDir, option, verbose = FALSE) {
  hook <- getOption(option)
  if (!is.function(hook)) {
    return()
  }

  if (verbose) {
    cat("Invoking `", option, "` hook\n", sep = "")
  }
  hook(appDir)
}

applicationDeleted <- function(client, deployment, recordPath, appMetadata) {
  header <- "Failed to find existing application on server; it's probably been deleted."
  not_interactive <- c(
    i = "Use {.fn forgetDeployment} to remove outdated record and try again.",
    i = "Or use {.fn applications} to see other applications you have on the server."
  )
  prompt <- "What do you want to do?"
  choices <- c(
    "Give up and try again later",
    "Delete existing deployment & create a new app"
  )

  cli_menu(header, prompt, choices, not_interactive = not_interactive, quit = 1)
  # Must be option 2

  path <- deploymentConfigFile(
    recordPath,
    deployment$name,
    deployment$account,
    deployment$server
  )
  unlink(path)

  accountDetails <- accountInfo(deployment$account, deployment$server)
  client$createApplication(
    deployment$name,
    deployment$title,
    "shiny",
    accountDetails$accountId,
    appMetadata$appMode
  )
}

# Does almost exactly the same work as writeManifest(), but called within
# deployApp() instead of being exposed to the user. Returns the path to the
# bundle directory, whereas writeManifest() returns nothing and deletes the
# bundle directory after writing the manifest.
bundleApp <- function(appName,
                      appDir,
                      appFiles,
                      appMetadata,
                      verbose = FALSE,
                      quiet = FALSE,
                      pythonConfig = NULL,
                      image = NULL,
                      envManagement = NULL,
                      envManagementR = NULL,
                      envManagementPy = NULL) {
  logger <- verboseLogger(verbose)

  # get application users (for non-document deployments)
  users <- NULL
  if (is.null(appMetadata$appPrimaryDoc)) {
    users <- suppressWarnings(authorizedUsers(appDir))
  }

  # copy files to bundle dir to stage
  logger("Bundling app dir")
  bundleDir <- bundleAppDir(
      appDir = appDir,
      appFiles = appFiles,
      appPrimaryDoc = appMetadata$appPrimaryDoc)
  defer(unlink(bundleDir, recursive = TRUE))

  # generate the manifest and write it into the bundle dir
  logger("Generate manifest.json")
  manifest <- createAppManifest(
    appDir = bundleDir,
    appMetadata = appMetadata,
    users = users,
    pythonConfig = pythonConfig,
    retainPackratDirectory = TRUE,
    image = image,
    envManagement = envManagement,
    envManagementR = envManagementR,
    envManagementPy = envManagementPy,
    verbose = verbose,
    quiet = quiet
  )
  manifestJson <- toJSON(manifest)
  manifestPath <- file.path(bundleDir, "manifest.json")
  writeLines(manifestJson, manifestPath, useBytes = TRUE)

  # create the bundle and return its path
  logger("Compressing the bundle")
  bundlePath <- tempfile("rsconnect-bundle", fileext = ".tar.gz")
  writeBundle(bundleDir, bundlePath)
  bundlePath
}

validURL <- function(url) {
  !(is.null(url) || url == "")
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

runStartupScripts <- function(appDir, quiet = FALSE, verbose = FALSE) {
  scripts <- c(
    # the site-wide startup script
    file.path(R.home("etc"), "rsconnect.site"),
    # the user startup script
    path.expand("~/.rsconnect_profile"),
    # a startup script specific to this application
    file.path(appDir, ".rsconnect_profile")
  )
  scripts <- scripts[file.exists(scripts)]

  # iterate over the startup scripts
  for (script in scripts) {
    taskStart(quiet, "Running {script}")

    env <- new_environment(parent = globalenv())
    source(script, verbose = verbose, local = env)
  }
}
