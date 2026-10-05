#' Maintain environment variables across multiple applications
#'
#' @description
#' * `listAccountEnvVars()` lists the environment variables used by
#'   every application published to the specified account.
#' * `updateAccountEnvVars()` updates the specified environment variables with
#'   their current values for every app that uses them.
#'
#' Secure environment variable are currently only supported by Posit Connect
#' so other server types will generate an error.
#'
#' Supported servers: Posit Connect servers
#'
#' @inheritParams deployApp
#' @export
#' @return `listAccountEnvVars()` returns a data frame with one row
#'   for each application. It has variables `id`, `guid`, `name`, and
#'   `envVars`. `envVars` is a list-column.
listAccountEnvVars <- function(server = NULL, account = NULL) {
  accountDetails <- accountInfo(account, server)
  client <- clientForAccount(accountDetails)
  checkServerHasEnvVars(client)

  apps <- applications(
    account = accountDetails$name,
    server = accountDetails$server
  )
  apps <- apps[c("id", "guid", "name")]

  envVars <- lapply(apps$guid, connectGetEnvVars, client = client)
  apps$envVars <- envVars
  apps
}

#' @export
#' @rdname listAccountEnvVars
#' @param envVars Names of environment variables to update. Their
#'   values will be automatically retrieved from the current process.
#'
#'   If you specify multiple environment variables, any application that
#'   uses any of them will be updated with all of them.
updateAccountEnvVars <- function(envVars, server = NULL, account = NULL) {
  check_character(envVars)

  accountDetails <- accountInfo(account, server)
  client <- clientForAccount(accountDetails)
  checkServerHasEnvVars(client)

  apps <- listAccountEnvVars(
    account = accountDetails$name,
    server = accountDetails$server
  )
  uses_vars <- vapply(apps$envVars, function(x) any(envVars %in% x), logical(1))
  if (!any(uses_vars)) {
    cli::cli_abort(
      "No applications use environment variable{?s} {.arg {envVars}}"
    )
  }

  guids <- apps$guid[uses_vars]
  cli::cli_progress_bar("Updating application...", total = length(guids))

  for (guid in guids) {
    connectSetEnvVars(client, guid, envVars)
    cli::cli_progress_update()
  }
}

# Helpers -----------------------------------------------------------------

checkServerHasEnvVars <- function(client, error_call = caller_env()) {
  if (supportsEnvVarManagement(client)) {
    return()
  }

  cli::cli_abort(
    "{serverDisplayName(client)} does not support environment variables",
    call = error_call
  )
}
