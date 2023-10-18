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

  # When appId is given, it identifies the target application.
  # - requires account+server (either explicit or inferred)
  # - when appName is also given, warn when different than the application name.
  # - discover the target application from the server using the account+server.
  # - should we merge with deployment record? probably yes, as envVars and metadata might be present on-disk?

  # When appName is not given, compute a default app name.
  # Using appName, discover deployment from on-disk records.
  # When no on-disk records, attempt to find content on the server with the same name.
  #   - prompt if we should target that server content.
  # When multiple on-disk records, ask for selection.

  if (!is.null(appId)) {
    # The appId identifies the target; appName may have been provided.
    return(deploymentTargetFromAppId(
      recordPath = recordPath,
      appId = appId,
      appName = appName,
      appTitle = appTitle,
      envVars = envVars,
      account = account,
      server = server,
      forceUpdate = forceUpdate,
      error_call = error_call
    ))
  }

  # The appName identifies the target; appId was not provided.
  if (!is.null(appName)) {
    return(deploymentTargetFromAppName(
      recordPath = recordPath,
      appName = appName,
      appTitle = appTitle,
      envVars = envVars,
      account = account,
      server = server,
      forceUpdate = forceUpdate,
      error_call = error_call
    ))
  }

  allDeployments <- deployments(
    appPath = recordPath,
    excludeOrphaned = FALSE
  )
  if (nrow(allDeployments) > 0) {
    deployment <- disambiguateDeployments(allDeployments, error_call = error_call)
    deployment <- updateDeploymentTarget(deployment, appTitle, envVars)
    # Account details are inferred from deployment when we have no incoming information.
    accountDetails <- accountInfo(deployment$account, deployment$server)
    return(list(
      accountDetails = accountDetails,
      deployment = deployment
    ))
  }

  
}

deploymentTargetFromAppId <- function(
  recordPath = ".",
  appId = NULL,
  appName = NULL,
  appTitle = NULL,
  envVars = NULL,
  account = NULL,
  server = NULL,
  forceUpdate = FALSE,
  error_call = caller_env()
) {
  # In order to use appId, we must have a target account+server.
  accountDetails <- findAccount(account, server)

  # If we have an on-disk deployment record, use it. Filter by server, but not account to include
  # collaborator-owned content.

  # Include "orphans" because the account that created the deployment record might not be the
  # current account.
  appDeployments <- deployments(
    appPath = recordPath,
    serverFilter = server,
    excludeOrphaned = FALSE
  )
  appDeployments <- appDeployments[appDeployments$id == appId, ]
  if (nrow(appDeployments) == 0) {
    # No on-disk deployment record for this server+id. Attempt to fetch this application from the
    # server.
    application <- getApplication(accountDetails, appId)
    # TODO: can application be NULL?
    deployment <- createDeploymentTarget(
      appName = application$name,
      appTitle = application$title %||% appTitle,
      appId = application$id,
      envVars = envVars,
      username = application$owner_username %||% accountDetails$name,
      # Note: account+server correspond to the deploying user that creates the deployment record
      # (DCF). We may not always have the true owning user account+server entry.
      account = accountDetails$name,
      server = accountDetails$server
    )
    if (deployment$name != appName) {
      cli::cli_abort(
        c(
          "Supplied {.arg appName} ({appName}) does not match deployment record ({deployment$name}).",
          i = "Omit {.arg appName} to use existing deployment for app {.str {appId}}, or",
          i = "Omit {.arg appId} to deploy using the name {.str {appName}}."
        ),
        call = error_call
      )
    }
    return(list(
      accountDetails = accountDetails,
      deployment = deployment
    ))
  } else if (nrow(appDeployments) == 1) {
    deployment <- appDeployments[1, ]
    if (!is.null(appName)) {
      deployment <- updateDeploymentTarget(deployment, appTitle, envVars)
      if (deployment$name != appName) {
        cli::cli_abort(
          c(
            "Supplied {.arg appName} ({appName}) does not match deployment record ({deployment$name}).",
            i = "Omit {.arg appName} to use existing deployment for app {.str {appId}}, or",
            i = "Omit {.arg appId} to deploy using the name {.str {appName}}."
          ),
          call = error_call
        )
      }
      return(list(
        accountDetails = accountDetails,
        deployment = deployment
      ))
    }
  } else {
    # Multiple rows with this id.
    # TODO: ERROR
  }
}  

deploymentTargetFromAppName <- function(
  recordPath = ".",
  appName = NULL,
  appTitle = NULL,
  envVars = NULL,
  account = NULL,
  server = NULL,
  forceUpdate = FALSE,
  error_call = caller_env()
) {

  allDeployments <- deployments(
    appPath = recordPath,
    nameFilter = appName,
    excludeOrphaned = FALSE
  )

  if (is.null(account) && is.null(server)) {
    # When no account or server ..
    if (nrow(allDeployments) > 0) {
      # When not provided account or server and we have existing deployments with this name, 
      deployment <- disambiguateDeployments(allDeployments, error_call = error_call)
      deployment <- updateDeploymentTarget(deployment, appTitle, envVars)
      # Account details are inferred from deployment when we have no incoming information.
      # TODO: Find an account matching on account+server, falling back to server.
      accountDetails <- accountInfo(deployment$account, deployment$server)
      return(list(
        accountDetails = accountDetails,
        deployment = deployment
      ))
    }
  }

  if (!is.null(account) && !is.null(server)) {
    # When account+server, match by account+server.
    matchingServer <- allDeployments$server == server
    matchingAccount <- allDeployments$account == account
    appDeployments <- allDeployments[matchingServer & matchingAccount, ]
    if (nrow(appDeployments) > 0) {
      accountDetails <- accountInfo(account, server)
      deployment <- disambiguateDeployments(appDeployments, error_call = error_call)
      deployment <- updateDeploymentTarget(deployment, appTitle, envVars)
      return(list(
        accountDetails = accountDetails,
        deployment = deployment
      ))
    }
  }

  if (!is.null(server)) {
    # When server, match by server (account match may have previously been attempted).
    # We may have (is.null(account)) or (!is.null(account)
    matchingServer <- allDeployments$server == server
    appDeployments <- allDeployments[matchingServer, ]
    if (nrow(appDeployments) > 0) {
      deployment <- disambiguateDeployments(appDeployments, error_call = error_call)
      deployment <- updateDeploymentTarget(deployment, appTitle, envVars)
      # TODO: What if account = NULL? Should we use the deployment to discover the account?
      # account <- account %||% deployment$account
      # We know that deployment$server==server.
      accountDetails <- accountInfo(account, server)
      return(list(
        accountDetails = accountDetails,
        deployment = deployment
      ))
    }
  }

  # no existing deployment is the requested target. check that this content has not been deployment
  # previously before creating a new item.
  appId <- NULL
  accountDetails <- accountInfo(account, server)
  if (!isPositCloudServer(accountDetails$server)) {
    existing <- applications(accountDetails$name, accountDetails$server)
    if (appName %in% existing$name) {
      app <- existing[existing$name == appName, , drop = FALSE]
      uniqueName <- findUnique(appName, existing$name)
      if (shouldUpdateApp(app, uniqueName, forceUpdate)) {
        appId <- app$id
        appName <- app$name
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
    username = accountDetails$name, # first deploy must be to own account
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
