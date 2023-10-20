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


  # When provided appId, attempt to find an existing deployment record with that identifier. Include
  # "orphaned" deployments because that record may have been created by some other user.
  #
  # The appId is the strongest association with some piece of content; it cannot be ignored.
  if (!is.null(appId)) {
    allDeployments <- deployments(
      appPath = recordPath,
      excludeOrphaned = FALSE
    )
    matchingDeployments <- allDeployments[allDeployments$appId == appId, ]
    if (nrow(matchingDeployments) == 0) {
      # Look for the application remotely. Requires a target account.
      accountDetails <- accountInfo(account, server)
      # This produces an error when the application id does not exist.
      application <- getApplication(accountDetails$name, accountDetails$server, appId)
      # Construct a deployment record from the remote application and the provided details.

      # Note: This deployment record includes the account+server that created this object, which
      # might not be the same person who created the application on the server. The owner username
      # tracks this distinction.

      createDeploymentTarget(
        appName = application$name %||% appName,
        appTitle = application$title %||% appTitle,
        appId = application$id,
        envVars = envVars,
        username = application$owner_username %||% accountDetails$name,
        account = accountDetails$name,
        server = accountDetails$server
      )
      return(list(
        accountDetails = accountDetails,
        deplyment = deployment
      ))
    } else if (nrow(matchingDeployments) == 1) {
      # Exactly one deployment. Discover target account from the deployment details and the incoming
      # choice.
      deployment <- matchingDeployments[1, ]

      server <- server %||% deployment$server
      # TODO: error when server is different from deployment server.
      
      # When account==NULL and deployment$account is a local account, use that.
      # When account==NULL and deployment$account is not a local account, use some other account on that server.
      # When account!=NULL, use that.

      # TODO: Transfer other incoming metadata. Possibly warn about inconsistent name?
      
      accountDetails <- accountInfo(account, server)
      return(list(
        accountDetails = accountDetails,
        deplyment = deployment
      ))
    } else {
      # Multiple matching deployments. Very unlikely, but it is possible that two servers share an identifier.
      
      
    }
  }

  # When provided appName, attempt to find an existing deployment record with that name.
  if (!is.null(appName)) {
    allDeployments <- deployments(
      appPath = recordPath,
      nameFilter = appName,
      excludeOrphaned = FALSE
    )

    if (nrow(matchingDeployments) == 0) {
      # No record of this name locally. Try to see if it is available remotely.
      
    } 
    

    matchingServer <- allDeployments$server
    
  }

  if (!is.null(account) || !is.null(server)) {
    if (is.null(account)) {
      # we have server but not account; see if a single account is identified.
      
    }
    if (is.null(server)) {
      # we have account but not server; see if a single account is identified.
      
    }
    
  }
  
  # when provided account and/or server
  #   when not account but server, see if the server identifies a single account
  #   when not server but account, see if there is one matching account
  #   when both account and server, hooray!

  #   generate app name from path
  #   see if that name exists remotely

  # when exactly one deployment, use it (assuming it is compatible with the account+server and available accounts)

  # when no existing deployments
  #  when account+server identifies a single account, use it
  #  otherwise, err.

  # when multiple existing deployments, prompt for selection.
  
  if (!is.null(appId) && is.null(appName)) {
    # User has supplied only appId, so retrieve app data from server
    # IDE supplies both appId and appName so should never hit this branch
    return(deploymentTargetForApp(
      appId = appId,
      appTitle = appTitle,
      account = account,
      server = server
    ))
  }

  # Use name/account/server to look up existing deployment;
  # create new deployment if no match found

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
