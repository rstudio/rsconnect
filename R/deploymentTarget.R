# calculate the deployment target based on the passed parameters and
# any saved deployments that we have
deploymentTarget <- function(recordPath = ".",
                             appId = NULL,
                             appName = NULL,
                             appTitle = NULL,
                             envVars = NULL,
                             account = NULL,
                             server = NULL,
                             forceUpdate = FALSE,
                             error_call = caller_env()) {

  appDeployments <- deployments(
    appPath = recordPath,
    nameFilter = appName,
    accountFilter = account,
    serverFilter = server
  )

  if (nrow(appDeployments) == 0) {
    fullAccount <- findAccount(account, server)
    if (is.null(appName)) {
      appName <- defaultAppName(recordPath, fullAccount$server)
    } else {
      check_string(appName, call = error_call)
    }

    appId <- NULL
    if (!isPositCloudServer(fullAccount$server)) {
      # Have we previously deployed elsewhere? We can't do this on cloud
      # because it assigns random app names (see #808 for details).
      existing <- applications(fullAccount$name, fullAccount$server)
      if (appName %in% existing$name) {
        thisApp <- existing[appName == existing$name, , drop = FALSE]
        uniqueName <- findUnique(appName, existing$name)

        if (shouldUpdateApp(thisApp, uniqueName, forceUpdate)) {
          appId <- thisApp$id
          appName <- thisApp$name
        } else {
          appName <- uniqueName
        }
      }
    }

    createDeploymentTarget(
      appName = appName,
      appTitle = appTitle,
      appId = appId,
      envVars = envVars,
      username = fullAccount$name, # first deploy must be to own account
      account = fullAccount$name,
      server = fullAccount$server
    )
  } else if (nrow(appDeployments) == 1) {
    # If both appName and appId supplied, check that they're consistent.
    if (!is.null(appId) && appDeployments$appId != appId) {
      cli::cli_abort(
        c(
          "Supplied {.arg appId} ({appId}) does not match deployment record ({appDeployments$appId}).",
          i = "Omit {.arg appId} to use existing for deployment for app {.str {appName}}, or",
          i = "Omit {.arg appName} to create new deployment record."
        ),
        call = error_call
      )
    }

    updateDeploymentTarget(appDeployments, appTitle, envVars)
  } else {
    deployment <- disambiguateDeployments(appDeployments, error_call = error_call)
    updateDeploymentTarget(deployment, appTitle, envVars)
  }
}

deploymentTargetForApp <- function(appId,
                                   appTitle = NULL,
                                   account = NULL,
                                   server = NULL) {
  accountDetails <- findAccount(account, server)
  application <- getApplication(accountDetails$name, accountDetails$server, appId)

  createDeploymentTarget(
    appName = application$name,
    appTitle = application$title %||% appTitle,
    appId = application$id,
    envVars = NULL,
    username = application$owner_username %||% accountDetails$name,
    account = accountDetails$name,
    server = accountDetails$server
  )
}

createDeploymentTarget <- function(appName,
                                   appTitle,
                                   appId,
                                   envVars,
                                   username,
                                   account,
                                   server,
                                   version = deploymentRecordVersion) {
  list(
    appName = appName,
    appTitle = appTitle %||% "",
    envVars = envVars,
    appId = appId,
    username = username,
    account = account,
    server = server,
    version = version
  )
}

updateDeploymentTarget <- function(previous, appTitle = NULL, envVars = NULL) {
  createDeploymentTarget(
    appName = previous$name,
    appTitle = appTitle %||% previous$title,
    appId = previous$appId,
    envVars = envVars %||% previous$envVars[[1]],
    # if username not previously recorded, use current account
    username = previous$username %||% previous$account,
    account = previous$account,
    server = previous$server,
    version = previous$version
  )
}

defaultAppName <- function(recordPath, server = NULL) {
  if (isDocumentPath(recordPath)) {
    name <- file_path_sans_ext(basename(recordPath))
    if (name == "index") {
      # parent directory will give more informative name
      name <- basename(dirname(recordPath))
    } else {
      # deploying a document
    }
  } else {
    # deploying a directory
    name <- basename(recordPath)
  }

  if (isShinyappsServer(server)) {
    # Replace non-alphanumerics with underscores, trim to length 64
    name <- tolower(gsub("[^[:alnum:]_-]+", "_", name, perl = TRUE))
    name <- gsub("_+", "_", name)
    if (nchar(name) > 64) {
      name <- substr(name, 1, 64)
    }
  }

  name
}

shouldUpdateApp <- function(application, uniqueName, forceUpdate = FALSE) {
  if (forceUpdate) {
    return(TRUE)
  }

  message <- c(
    "Discovered a previously deployed app named {.str {application$name}}",
    "(View it at {.url {application$url}})"
  )

  prompt <- "What do you want to do?"

  choices <- c(
    "Update the existing app.",
    "Create a new app with automatically generated name ({.str {uniqueName}}).",
    "Abort this deployment and supply a custom `appName`."
  )

  not_interactive <- c(
    i = "Set `forceUpdate = TRUE` to update it.",
    i = "Supply a unique `appName` to deploy a new application."
  )

  cli_menu(message, prompt, choices, not_interactive, quit = 3) == 1
}


findUnique <- function(x, existing) {
  i <- 1
  name <- paste0(x, "-", i)

  while (name %in% existing) {
    i <- i + 1
    name <- paste0(x, "-", i)
  }

  name
}
