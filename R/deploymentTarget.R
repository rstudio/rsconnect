# calculate the deployment target based on the passed parameters and
# any saved deployments that we have
deploymentTarget <- function(recordPath = ".",
                             appId = NULL,
                             appName = NULL,
                             appTitle = NULL,
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
    # Have we previously deployed elsewhere?
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

    createDeploymentTarget(
      appName,
      appTitle,
      appId,
      fullAccount$name, # first deploy must be to own account
      fullAccount$name,
      fullAccount$server
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

    updateDeploymentTarget(appDeployments, appTitle)
  } else {
    apps <- paste0(
      appDeployments$name, " ",
      "(", accountLabel(appDeployments$account, appDeployments$server), "): ",
      "{.url ", appDeployments$url, "}"
    )
    not_interactive <- c(
      "Please use {.arg appName}, {.arg server} or {.arg account} to disambiguate.",
      "Known applications:",
      set_names(apps, "*")
    )
    idx <- cli_menu(
      "This directory has been previously deployed in multiple places.",
      "Which deployment do you want to use?",
      choices = apps,
      not_interactive = not_interactive,
      error_call = error_call
    )
    updateDeploymentTarget(appDeployments[idx, ], appTitle)
  }
}


deploymentTargetForApp <- function(appId,
                                   appTitle = NULL,
                                   account = NULL,
                                   server = NULL) {
  accountDetails <- findAccount(account, server)
  application <- getApplication(accountDetails$name, accountDetails$server, appId)

  createDeploymentTarget(
    application$name,
    application$title %||% appTitle,
    application$id,
    application$owner_username,
    accountDetails$name,
    accountDetails$server
  )
}

createDeploymentTarget <- function(appName,
                                   appTitle,
                                   appId,
                                   username,
                                   account,
                                   server) {
  list(
    appName = appName,
    appTitle = appTitle %||% "",
    appId = appId,
    username = username,
    account = account,
    server = server
  )
}

updateDeploymentTarget <- function(previous, appTitle = NULL) {
  createDeploymentTarget(
    previous$name,
    appTitle %||% previous$title,
    previous$appId,
    # if username not previously recorded, use current account
    previous$username %||% previous$account,
    previous$account,
    previous$server
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
