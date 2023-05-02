#' List Application Deployments
#'
#' List deployment records for a given application.
#' @param appPath The path to the content that was deployed, either a directory
#'   or an individual document.
#' @param nameFilter Return only deployments matching the given name (optional)
#' @param accountFilter Return only deployments matching the given account
#'   (optional)
#' @param serverFilter Return only deployments matching the given server
#'   (optional)
#' @param excludeOrphaned If `TRUE` (the default), return only deployments
#'   made by a currently registered account. Deployments made from accounts that
#'   are no longer registered (via e.g.[removeAccount()]) will not be
#'   returned.
#' @return
#' Returns a data frame with at least following columns:
#' \tabular{ll}{
#' `name` \tab Name of deployed application\cr
#' `account` \tab Account owning deployed application\cr
#' `bundleId` \tab Identifier of deployed application's bundle\cr
#' `url` \tab URL of deployed application\cr
#' `deploymentFile` \tab Name of configuration file\cr
#' }
#'
#' If additional metadata has been saved with the deployment record using the
#' `metadata` argument to [deployApp()], the frame will include
#' additional columns.
#'
#' @examples
#' \dontrun{
#'
#' # Return all deployments of the ~/r/myapp directory made with the 'abc'
#' # account
#' deployments("~/r/myapp", accountFilter="abc")
#' }
#' @seealso [applications()] to get a list of deployments from the
#'   server, and [deployApp()] to create a new deployment.
#' @export
deployments <- function(appPath = ".",
                        nameFilter = NULL,
                        accountFilter = NULL,
                        serverFilter = NULL,
                        excludeOrphaned = TRUE) {

  migrateDeploymentsConfig(appPath)
  paths <- deploymentConfigFiles(appPath)

  dcf <- lapply(paths, read.dcf)
  dcf <- lapply(dcf, as.data.frame, stringsAsFactors = FALSE)

  deployments <- rbind_fill(dcf, deploymentFields)
  deployments$deploymentFile <- paths

  # Apply filters
  ok <- rep(TRUE, nrow(deployments))
  if (!is.null(nameFilter)) {
    ok <- ok & deployments$name == nameFilter
  }
  if (!is.null(accountFilter)) {
    ok <- ok & deployments$account == accountFilter
  }
  if (!is.null(serverFilter)) {
    ok <- ok & deployments$server == serverFilter
  }
  if (excludeOrphaned) {
    activeAccounts <- accounts()
    activeAccountServers <- paste0(activeAccounts$server, "@", activeAccounts$name)
    accountServer <- paste0(deployments$server, "@", deployments$account)
    okServer <- isRPubs(deployments$server) | accountServer %in% activeAccountServers
    ok <- ok & okServer
  }

  deployments$envVars[is.na(deployments$envVars)] <- ""
  if (is.character(deployments$envVars)) {
    deployments$envVars <- strsplit(deployments$envVars, ", ")
  }

  deployments[ok, , drop = FALSE]
}

deploymentFields <- c(
  "name", "title", "username", "account", "server", "hostUrl", "appId",
  "bundleId", "url", "envVars", "version"
)

deploymentRecordVersion <- 1L

saveDeployment <- function(recordDir,
                           target,
                           application,
                           bundleId = NULL,
                           hostUrl = serverInfo(target$server)$url,
                           metadata = list(),
                           addToHistory = TRUE) {
  deployment <- deploymentRecord(
    name = target$appName,
    title = target$appTitle,
    username = target$username,
    account = target$account,
    server = target$server,
    envVars = target$envVars,
    version = target$version,
    hostUrl = hostUrl,
    appId = application$id,
    bundleId = bundleId,
    url = application$url,
    metadata = metadata
  )
  path <- deploymentConfigFile(recordDir, target$appName, target$account, target$server)
  writeDeploymentRecord(deployment, path)

  # also save to global history
  if (addToHistory) {
    addToDeploymentHistory(recordDir, deployment)
  }

  invisible(path)
}

deploymentRecord <- function(name,
                             title,
                             username,
                             account,
                             server,
                             envVars = NULL,
                             hostUrl = NULL,
                             appId = NULL,
                             bundleId = NULL,
                             url = NULL,
                             version = deploymentRecordVersion,
                             metadata = list()) {

  check_character(envVars, allow_null = TRUE)

  standard <- list(
    name = name,
    title = title %||% "",
    username = username,
    account = account,
    server = server,
    envVars = if (length(envVars) > 0) paste0(envVars, collapse = ", ") else NA,
    hostUrl = hostUrl %||% "",
    appId = appId %||% "",
    bundleId = bundleId %||% "",
    url = url %||% "",
    version = version
  )
  c(standard, metadata)
}

writeDeploymentRecord <- function(record, filePath) {
  # use a long width so URLs don't line-wrap
  write.dcf(record, filePath, width = 4096)
}

# Workbench uses to show a list of recently deployed content on user dashboard
addToDeploymentHistory <- function(appPath, deploymentRecord) {
  # add the appPath to the deploymentRecord
  deploymentRecord$appPath <- appPath

  # write new history file
  newHistory <- deploymentHistoryPath(new = TRUE)
  writeDeploymentRecord(deploymentRecord, newHistory)

  history <- deploymentHistoryPath()
  # append existing history to new history
  if (file.exists(history)) {
    cat("\n", file = newHistory, append = TRUE)
    file.append(newHistory, history)
  }

  # overwrite with new history
  file.rename(newHistory, history)
  invisible()
}

#' Forget Application Deployment
#'
#' Forgets about an application deployment. This is useful if the application
#' has been deleted on the server, or the local deployment information needs to
#' be reset.
#'
#' @param appPath The path to the content that was deployed, either a directory
#'   or an individual document.
#' @param name The name of the content that was deployed (optional)
#' @param account The name of the account to which the content was deployed
#'   (optional)
#' @param server The name of the server to which the content was deployed
#'   (optional)
#' @param dryRun Set to TRUE to preview the files/directories to be removed
#'   instead of actually removing them. Defaults to FALSE.
#' @param force Set to TRUE to remove files and directories without prompting.
#'   Defaults to FALSE in interactive sessions.
#' @return NULL, invisibly.
#'
#' @details This method removes from disk the file containing deployment
#'   metadata. If "name", "account", and "server" are all NULL, then all of the
#'   deployments for the application are forgotten; otherwise, only the
#'   specified deployment is forgotten.
#'
#' @export
forgetDeployment <- function(appPath = getwd(), name = NULL,
                             account = NULL, server = NULL,
                             dryRun = FALSE, force = !interactive()) {
  if (is.null(name) && is.null(account) && is.null(server)) {
    dcfDir <- deploymentConfigDir(appPath)
    if (dryRun)
      message("Would remove the directory ", dcfDir)
    else if (file.exists(dcfDir)) {
      if (!force) {
        prompt <- paste("Forget all deployment records for ", appPath, "? [Y/n] ", sep = "")
        input <- readline(prompt)
        if (nzchar(input) && !identical(input, "y") && !identical(input, "Y"))
          stop("No deployment records removed.", call. = FALSE)
      }
      unlink(dcfDir, recursive = TRUE)
    } else {
      message("No deployments found for the application at ", appPath)
    }
  } else {
    if (is.null(name) || is.null(account) || is.null(server)) {
      stop("Invalid argument. ",
           "Supply the name, account, and server of the deployment record to delete. ",
           "Supply NULL for all three to delete all deployment records.")
    }
    dcf <- deploymentConfigFile(appPath, name, account, server)
    if (dryRun)
      message("Would remove the file ", dcf)
    else if (file.exists(dcf)) {
      if (!force) {
        prompt <- paste("Forget deployment of ", appPath, " to '", name, "' on ",
                        server, "? [Y/n] ", sep = "")
        input <- readline(prompt)
        if (nzchar(input) && !identical(input, "y") && !identical(input, "Y"))
          stop("Cancelled. No deployment records removed.", call. = FALSE)
      }
      unlink(dcf)
    } else {
      message("No deployment of ", appPath, " to '", name, "' on ", server,
              " found.")
    }
  }

  invisible(NULL)
}
