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
#' @inheritParams deployApp
#' @export
#' @return `listAccountEnvVars()` returns a data frame with one row
#'   for each data frame. It has variables `id`, `guid`, `name`, and
#'   `envVars`. `envVars` is a list-column.
listAccountEnvVars <- function(server = NULL, account = NULL) {
  accountDetails <- accountInfo(account, server)
  checkServerHasEnvVars(accountDetails$server)

  apps <- applications(
    account = accountDetails$name,
    server = accountDetails$server
  )
  apps <- apps[c("id", "guid", "name")]

  client <- clientForAccount(accountDetails)
  envVars <- lapply(apps$guid, client$getEnvVars)
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
  checkServerHasEnvVars(accountDetails$server)

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

  client <- clientForAccount(accountDetails)
  for (guid in guids) {
    client$setEnvVars(guid, envVars)
    cli::cli_progress_update()
  }
}

# Helpers -----------------------------------------------------------------

checkServerHasEnvVars <- function(server, error_call = caller_env()) {
  if (isConnectServer(server)) {
    return()
  }

  cli::cli_abort(
    "The {.arg server} {.str server} does not support environment variables"
  )
}
