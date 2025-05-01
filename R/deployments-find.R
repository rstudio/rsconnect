findDeployment <- function(
  appPath = getwd(),
  appName = NULL,
  server = NULL,
  account = NULL,
  error_call = caller_env()
) {
  deps <- deployments(
    appPath,
    nameFilter = appName,
    serverFilter = server,
    accountFilter = account
  )

  if (nrow(deps) == 0) {
    # When the name and account information does not discover a single deployment, return a skeleton
    # deployment object, which can subsequently be used to query the service.

    # Infers account when not provided...
    accountDetails <- accountInfo(account, server)
    if (is.null(appName)) {
      appName <- generateAppName(
        NULL,
        appPath,
        account = accountDetails$name,
        unique = FALSE
      )
    }
    list(
      name = appName,
      account = accountDetails$name,
      server = accountDetails$server
    )
  } else if (nrow(deps) == 1) {
    as.list(deps)
  } else {
    disambiguateDeployments(deps, error_call = error_call)
  }
}

disambiguateDeployments <- function(appDeployments, error_call = caller_env()) {
  if (nrow(appDeployments) == 1) {
    return(appDeployments[1, ])
  }

  apps <- sprintf(
    "%s (%s): {.url %s}",
    appDeployments$name,
    accountLabel(appDeployments$account, appDeployments$server),
    appDeployments$url
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
  as.list(appDeployments[idx, ])
}
