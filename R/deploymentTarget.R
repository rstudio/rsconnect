# calculate the deployment target based on the passed parameters and
# any saved deployments that we have
deploymentTarget <- function(recordPath = ".",
                             appName = NULL,
                             appTitle = NULL,
                             appId = NULL,
                             account = NULL,
                             server = NULL,
                             quiet = FALSE,
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
      appName <- generateAppName(appTitle, recordPath, account, unique = FALSE)
    }
    if (!quiet) {
      dest <- accountId(fullAccount$name, fullAccount$server)
      cli::cli_alert_info("Deploying {.val {appName}} to {.val {dest}}")
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
    if (!quiet) {
      name <- appDeployments$name
      dest <- accountId(appDeployments$username, appDeployments$server)
      cli::cli_alert_info("Re-deploying {.val {name}} to {.val {dest}}")
    }

    createDeploymentTarget(
      appDeployments$name,
      appTitle %||% appDeployments$title,
      appDeployments$appId,
      # if username not previously recorded, use current account
      appDeployments$username %||% appDeployments$account,
      appDeployments$account,
      appDeployments$server
    )
  } else {
    cli::cli_abort(
      c(
        "This app has been previously deployed in multiple places.",
        "Please use {.arg appName}, {.arg server} or {.arg account} to disambiguate.",
        i = "Known application names: {.str {unique(appDeployments$name)}}.",
        i = "Known servers: {.str {unique(appDeployments$server)}}.",
        i = "Known account names: {.str {unique(appDeployments$account)}}."
      ),
      call = error_call
    )
  }
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
