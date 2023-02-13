# calculate the deployment target based on the passed parameters and
# any saved deployments that we have
deploymentTarget <- function(appPath = ".",
                             appName = NULL,
                             appTitle = NULL,
                             appId = NULL,
                             account = NULL,
                             server = NULL,
                             error_call = caller_env()) {
  # read existing deployments
  appDeployments <- deployments(appPath = appPath)

  # if appTitle specified but not appName, generate name from title
  if (is.null(appName) && !is.null(appTitle) && nzchar(appTitle)) {
    appName <- generateAppName(appTitle, appPath, account, unique = FALSE)
  }

  # both appName and account explicitly specified
  if (!is.null(appName) && !is.null(account)) {
    # TODO(HW): why does this always ignore existing deployments?
    fullAccount <- findAccount(account, server, error_call = error_call)
    createDeploymentTarget(
      appPath,
      appName,
      appTitle,
      appId,
      fullAccount$name,
      account,
      fullAccount$server
    )
  }

  # just appName specified
  else if (!is.null(appName)) {
    # find any existing deployments of this application
    appDeployments <- deployments(
      appPath,
      nameFilter = appName
    )

    # if there are none then we can create it if there is a single account
    # registered that we can default to
    if (nrow(appDeployments) == 0) {
      fullAccount <- findAccount(account, server, error_call = error_call)
      createDeploymentTarget(
        appPath,
        appName,
        appTitle,
        appId,
        fullAccount$name,
        accounts,
        fullAccount$server
      )
    }

    # single existing deployment
    else if (nrow(appDeployments) == 1) {
      # TODO(HW): why doesn't this take appId from the existing deployment?
      createDeploymentTarget(
        appPath,
        appName,
        appTitle,
        appId,
        usernameFromDeployment(appDeployments),
        appDeployments$account,
        appDeployments$server
      )
    }

    # multiple existing deployments
    else if (nrow(appDeployments) > 1) {
      cli::cli_abort(
        c(
          "This account has been previously deployed in multiple places.",
          "Please use {.arg server} or {.arg account} to disambiguate.",
          i = "Known servers: {.str {unique(appDeployments$server)}}.",
          i = "Known account names: {.str {unique(appDeployments$name)}}."
        ),
        call = error_call
      )
    }
  }

  # just account/server specified, that's fine we just default the app name
  # based on the basename of the application directory
  else if (!is.null(account) || !is.null(server)) {
    fullAccount <- findAccount(account, server)
    createDeploymentTarget(
      appPath,
      generateAppName(appTitle, appPath, account, unique = FALSE),
      appTitle,
      appId,
      fullAccount$name,
      account,
      fullAccount$server
    )
  }

  # neither specified but a single existing deployment
  else if (nrow(appDeployments) == 1) {
    createDeploymentTarget(
      appPath,
      appDeployments$name,
      appDeployments$title,
      appDeployments$appId,
      usernameFromDeployment(appDeployments),
      appDeployments$account,
      appDeployments$server
    )
  }

  # neither specified and no existing deployments
  else if (nrow(appDeployments) == 0) {
    fullAccount <- findAccount(account, server)
    createDeploymentTarget(
      appPath,
      generateAppName(appTitle, appPath, account, unique = FALSE),
      appTitle,
      appId,
      fullAccount$name,
      accounts,
      fullAccount$server
    )
  }

  # neither specified and multiple existing deployments
  else {
    stop("Unable to deploy using default arguments (multiple existing ",
      "deployments from this application directory already exist). ",
      "Please specify appName and/or account name explicitly.",
      call. = FALSE
    )
  }
}

# function to compute the target username from a deployment
usernameFromDeployment <- function(appDeployments) {
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
createDeploymentTarget <- function(appPath,
                                   appName,
                                   appTitle,
                                   appId,
                                   username,
                                   account,
                                   server) {
  if (is.null(appId)) {
    appId <- findAppId(
      appPath,
      appName,
      account = account,
      server = server,
      username = username
    )
  }

  list(
    appName = appName,
    appTitle = appTitle,
    appId = appId,
    username = username,
    account = account,
    server = server
  )
}

findAppId <- function(appPath, appName, account, server, username) {
  appId <- NULL
  serverDetails <- serverInfo(server)

  existingDeployments <- deployments(appPath, nameFilter = appName)
  for (i in seq_len(nrow(existingDeployments))) {
    if (identical(existingDeployments[[i, "account"]], account) &&
      identical(existingDeployments[[i, "server"]], server)) {
      # account and server matches a locally configured account
      appId <- existingDeployments[[i, "appId"]]
      break
    } else if (identical(existingDeployments[[i, "username"]], username) &&
      identical(existingDeployments[[i, "host"]], serverDetails$url)) {
      # username and host match the user and host we're deploying to
      appId <- existingDeployments[[i, "appId"]]
      break
    }
  }

  appId
}
