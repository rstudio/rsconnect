findDeployment <- function(appPath = ".",
                           appName = NULL,
                           server = NULL,
                           account = NULL,
                           error_call = caller_env()) {
  deps <- deployments(
    appPath,
    nameFilter = appName,
    serverFilter = server,
    accountFilter = account
  )

  if (nrow(deps) == 0) {
    cli::cli_abort(
      "Couldn't find any deployments matching supplied criteria.",
      call = error_call
    )
  } else if (nrow(deps) == 1) {
    as.list(deps)
  } else {
    disambiguateDeployments(deps, error_call = error_call)
  }
}

disambiguateDeployments <- function(appDeployments, error_call = caller_env()) {
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
  as.list(appDeployments[idx, ])
}
