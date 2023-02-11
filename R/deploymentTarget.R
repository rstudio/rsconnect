# calculate the deployment target based on the passed parameters and
# any saved deployments that we have
deploymentTarget <- function(appPath,
                             appName,
                             appTitle,
                             appId,
                             account,
                             server = NULL) {
  # read existing accounts
  accounts <- accounts(server)[, "name"]
  if (length(accounts) == 0) {
    stopWithNoAccount()
  }

  # validate account if provided
  if (!is.null(account)) {
    if (!account %in% accounts) {
      stop(
        paste("Unknown account name '", account, "' (you can use the ",
          "setAccountInfo function to add a new account)",
          sep = ""
        ),
        call. = FALSE
      )
    }
  }

  # read existing deployments
  appDeployments <- deployments(appPath = appPath)


  # if appTitle specified but not appName, generate name from title
  if (is.null(appName) && !is.null(appTitle) && nzchar(appTitle)) {
    appName <- generateAppName(appTitle, appPath, account, unique = FALSE)
  }

  # both appName and account explicitly specified
  if (!is.null(appName) && !is.null(account)) {
    accountDetails <- accountInfo(account, server)
    createDeploymentTarget(
      appPath,
      appName,
      appTitle,
      appId,
      accountDetails$username,
      account,
      accountDetails$server
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
      if (length(accounts) == 1) {
        # read the server associated with the account
        accountDetails <- accountInfo(accounts, server)
        createDeploymentTarget(
          appPath,
          appName,
          appTitle,
          appId,
          accountDetails$username,
          accounts,
          accountDetails$server
        )
      } else {
        stopWithSpecifyAccount()
      }
    }

    # single existing deployment
    else if (nrow(appDeployments) == 1) {
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
      stop(
        paste(
          "Please specify the account you want to deploy '", appName,
          "' to (you have previously deployed this application to ",
          "more than one account).",
          sep = ""
        ),
        call. = FALSE
      )
    }
  }

  # just account/server specified, that's fine we just default the app name
  # based on the basename of the application directory
  else if (!is.null(account) || !is.null(server)) {
    if (is.null(account)) {
      account <- accounts(server)[, "name"]
      if (length(account) > 1) {
        stopWithSpecifyAccount()
      }
    }
    accountDetails <- accountInfo(account, server)
    createDeploymentTarget(
      appPath,
      generateAppName(appTitle, appPath, account, unique = FALSE),
      appTitle,
      appId,
      accountDetails$username,
      account,
      accountDetails$server
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
    # single account we can default to
    if (length(accounts) == 1) {
      accountDetails <- accountInfo(accounts)
      createDeploymentTarget(
        appPath,
        generateAppName(appTitle, appPath, account, unique = FALSE),
        appTitle,
        appId,
        accountDetails$username,
        accounts,
        accountDetails$server
      )
    } else {
      stop(
        "Please specify the account and server to which you want to deploy ",
        "the application (there is more than one account registered ",
        "on this system).",
        call. = FALSE
      )
    }
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
  # look up the server URL
  serverDetails <- serverInfo(server)

  # look for an application ID if we weren't supplied one
  if (is.null(appId)) {
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
