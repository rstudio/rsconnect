# Discover the deployment target given the passed information.
#
# Returns a list containing a deployment record and the account details to use
# when performing the deployment.
#
# When appId is provided, it must identify an existing application. The
# application may have been created by some other user. That application may
# or may not have an existing deployment record on disk. It is an error when
# appId does not identify an application.
#
# When appName is provided, it may identify an existing application owned by
# the calling user (e.g. associated with a locally known account).
#
# Without appId or appName to identify an existing deployment, local
# deployment records are considered before falling back to a generated name.
#
# When the targeted name does not exist, a deployment record with NULL appId
# is returned, which signals to the caller that an application should be
# created.
findDeploymentTarget <- function(
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

  if (!is.null(appId)) {
    return(findDeploymentTargetByAppId(
      recordPath = recordPath,
      appId = appId,
      appName = appName,
      appTitle = appTitle,
      envVars = envVars,
      account = account,
      server = server,
      error_call = error_call
    ))
  }

  if (!is.null(appName)) {
    return(findDeploymentTargetByAppName(
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

  # No identifying appId or appName.

  # When there are existing deployments, ask the user to select one and use
  # it. Only deployments associated with locally configured account+server
  # combinations are considered.
  allDeployments <- deployments(
    appPath = recordPath,
    accountFilter = account,
    serverFilter = server
  )
  if (nrow(allDeployments) > 0) {
    deployment <- disambiguateDeployments(allDeployments, error_call = error_call)
    deployment <- updateDeployment(deployment, appTitle, envVars)
    accountDetails <- accountInfo(deployment$account, deployment$server)
    return(list(
      accountDetails = accountDetails,
      deployment = deployment
    ))
  }

  # Otherwise, identify a target account (given just one available or prompted
  # by the user), generate a name, and locate the deployment.
  accountDetails <- accountInfo(account, server)
  appName <- generateAppName(appTitle, recordPath, accountDetails$name, unique = FALSE)
  return(findDeploymentTargetByAppName(
    recordPath = recordPath,
    appName = appName,
    appTitle = appTitle,
    envVars = envVars,
    account = accountDetails$name,
    server = accountDetails$server,
    forceUpdate = forceUpdate,
    error_call = error_call
  ))
}

# Discover the deployment target given appId.
#
# When appId is provided, all other information is secondary. An appId is an indication from the
# caller that the content has already been deployed elsewhere. If we cannot locate that content,
# deployment fails.
#
# The target content may have been created by some other user; the account for this session may
# differ from the account used when creating the content.
findDeploymentTargetByAppId <- function(
  recordPath = ".",
  appId = NULL,
  appName = NULL,
  appTitle = NULL,
  envVars = NULL,
  account = NULL,
  server = NULL,
  error_call = caller_env()
) {

  # We must have a target account+server in order to use the appId.
  # The selected account may not be the original creator of the content.
  accountDetails <- accountInfo(account, server)

  # Filtering is only by server and includes all deployments in case we have a deployment record
  # from a collaborator.
  appDeployments <- deployments(
    appPath = recordPath,
    serverFilter = server,
    excludeOrphaned = FALSE
  )
  appDeployments <- appDeployments[appDeployments$appId == appId, ]
  if (nrow(appDeployments) > 1) {
    cli::cli_abort(
      c(
        "Supplied {.arg appId} ({appId}) identifies multiple deployments.",
        i = "Manage obsolete deployments with rsconnect::forgetDeployment()."
      ),
      call = error_call
    )
  }

  # Existing local deployment record.
  if (nrow(appDeployments) == 1) {
    deployment <- appDeployments[1, ]
    deployment <- updateDeployment(deployment, appTitle, envVars)
    return(list(
      accountDetails = accountDetails,
      deployment = deployment
    ))
  }

  # No local deployment record. Get it from the server.
  application <- getApplication(accountDetails$name, accountDetails$server, appId)

  # Note: The account+server of this deployment record may
  # not correspond to the original content creator.
  deployment <- createDeployment(
    appName = application$name,
    appTitle = application$title %||% appTitle,
    appId = application$id,
    envVars = envVars,
    username = application$owner_username %||% accountDetails$name,
    account = accountDetails$name,
    server = accountDetails$server
  )

  return(list(
    accountDetails = accountDetails,
    deployment = deployment
  ))
}

# Discover the deployment target given appName.
#
# When appName is provided it identifies content previously created by a locally configured account.
#
# The account details from the deployment record identify the final credentials we will use, as
# account+server may not have been specified by the caller.
findDeploymentTargetByAppName <- function(
  recordPath = ".",
  appName = NULL,
  appTitle = NULL,
  envVars = NULL,
  account = NULL,
  server = NULL,
  forceUpdate = FALSE,
  error_call = caller_env()
) {

  appDeployments <- deployments(
    appPath = recordPath,
    nameFilter = appName,
    accountFilter = account,
    serverFilter = server
  )

  # When the appName along with the (optional) account+server identifies exactly one previous
  # deployment, use it.
  if (nrow(appDeployments) == 1) {
    deployment <- appDeployments[1, ]
    deployment <- updateDeployment(deployment, appTitle, envVars)
    accountDetails <- accountInfo(deployment$account, deployment$server)
    return(list(
      accountDetails = accountDetails,
      deployment = deployment
    ))
  }

  # When the appName identifies multiple targets, we may not have had an account+server constraint.
  # Ask the user to choose.
  if (nrow(appDeployments) > 1) {
    deployment <- disambiguateDeployments(appDeployments, error_call = error_call)
    deployment <- updateDeployment(deployment, appTitle, envVars)
    accountDetails <- accountInfo(deployment$account, deployment$server)
    return(list(
      accountDetails = accountDetails,
      deployment = deployment
    ))
  }

  # When the appName does not identify a target, see if it exists on the server. That content is
  # conditionally used. A resolved account is required.
  accountDetails <- accountInfo(account, server)
  if (!isPositCloudServer(accountDetails$server)) {
    client <- clientForAccount(accountDetails)
    application <- tryCatch(
      getAppByName(client, accountDetails, appName),
      rsconnect_app_not_found = function(err) NULL
    )
    if (!is.null(application)) {
      uniqueName <- findUnique(appName, application$name)
      if (shouldUpdateApp(application, uniqueName, forceUpdate)) {
        deployment <- createDeployment(
          appName = application$name,
          appTitle = application$title %||% appTitle,
          appId = application$id,
          envVars = envVars,
          username = application$owner_username %||% accountDetails$name,
          account = accountDetails$name,
          server = accountDetails$server
        )
        return(list(
          accountDetails = accountDetails,
          deployment = deployment
        ))
      } else {
        appName <- uniqueName
      }
    }
  }

  # No existing target, or the caller does not want to re-use that content.
  deployment <- createDeployment(
    appName = appName,
    appTitle = appTitle,
    appId = NULL,
    envVars = envVars,
    username = accountDetails$name,
    account = accountDetails$name,
    server = accountDetails$server
  )
  return(list(
    accountDetails = accountDetails,
    deployment = deployment
  ))
}

createDeployment <- function(appName,
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

updateDeployment <- function(previous, appTitle = NULL, envVars = NULL) {
  createDeployment(
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
