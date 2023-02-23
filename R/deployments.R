saveDeployment <- function(recordDir,
                           target,
                           application,
                           bundleId,
                           hostUrl = serverInfo(target$server)$hostUrl,
                           metadata = list()) {

  # create the record to write to disk
  deployment <- deploymentRecord(
    name = target$appName,
    title = target$appTitle %||% "",
    username = target$username,
    account = target$account,
    server = target$server,
    hostUrl = hostUrl,
    appId = application$id,
    bundleId = bundleId,
    url = application$url,
    when = as.numeric(Sys.time()),
    lastSyncTime = as.numeric(Sys.time()),
    metadata = metadata
  )

  path <- deploymentConfigFile(recordDir, target$appName, target$account, target$server)
  writeDeploymentRecord(deployment, path)

  # also save to global history
  addToDeploymentHistory(recordDir, deployment)

  invisible(NULL)
}

writeDeploymentRecord <- function(record, filePath) {
  # use a long width so URLs don't line-wrap
  write.dcf(record, filePath, width = 4096)
}

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
#' `when` \tab When the application was deployed (in seconds since the
#'   epoch)\cr
#' `lastSyncTime` \tab When the application was last synced (in seconds since the
#'   epoch)\cr
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
deployments <- function(appPath, nameFilter = NULL, accountFilter = NULL,
                        serverFilter = NULL, excludeOrphaned = TRUE) {

  migrateDeploymentsConfig(appPath)

  # calculate rsconnect dir
  rsconnectDir <- deploymentConfigDir(appPath)

  # build list of deployment records
  deploymentRecs <- deploymentRecord(name = character(),
                                     title = character(),
                                     username = character(),
                                     account = character(),
                                     server = character(),
                                     hostUrl = character(),
                                     appId = character(),
                                     bundleId = character(),
                                     url = character(),
                                     when = numeric(),
                                     lastSyncTime = numeric())

  # get list of active accounts
  activeAccounts <- accounts()

  for (deploymentFile in list.files(rsconnectDir, glob2rx("*.dcf"),
                                    recursive = TRUE)) {

    # derive account and server name from deployment record location
    account <- basename(dirname(deploymentFile))
    server <- basename(dirname(dirname(deploymentFile)))

    # apply optional server filter
    if (!is.null(serverFilter) && !identical(serverFilter, server))
      next

    # apply optional account filter
    if (!is.null(accountFilter) && !identical(accountFilter, account))
      next

    # apply optional name filter
    name <- file_path_sans_ext(basename(deploymentFile))
    if (!is.null(nameFilter) && !identical(nameFilter, name))
      next

    # exclude orphaned if requested (note that the virtual server "rpubs.com"
    # is always considered to be registered)
    if (excludeOrphaned && server != "rpubs.com") {
      # orphaned by definition if we have no accounts registered
      if (is.null(activeAccounts) || identical(nrow(activeAccounts), 0))
        next

      # filter by account name and then by server
      matchingAccounts <- activeAccounts[activeAccounts[["name"]] == account, ]
      matchingAccounts <-
        matchingAccounts[matchingAccounts[["server"]] == server, ]

      # if there's no account with the given name and server, consider this
      # record to be an orphan
      if (nrow(matchingAccounts) == 0)
        next
    }

    # parse file
    deployment <- as.data.frame(readDcf(file.path(rsconnectDir, deploymentFile)),
                                stringsAsFactors = FALSE)

    # fill in any columns missing in this record
    missingCols <- setdiff(colnames(deploymentRecs), colnames(deployment))
    if (length(missingCols) > 0) {
      deployment[, missingCols] <- NA
    }

    # if this record contains any columns that aren't present everywhere, add
    # them
    extraCols <- setdiff(colnames(deployment), colnames(deploymentRecs))
    if (length(extraCols) > 0 && nrow(deploymentRecs) > 0) {
      deploymentRecs[, extraCols] <- NA
    }

    # record the deployment file for metadata management
    deployment$deploymentFile <- file.path(rsconnectDir, deploymentFile)

    # append to record set to return
    deploymentRecs <- rbind(deploymentRecs, deployment)
  }

  deploymentRecs
}

deploymentRecord <- function(name,
                             title,
                             username,
                             account,
                             server,
                             hostUrl,
                             appId,
                             bundleId,
                             url,
                             when,
                             lastSyncTime = as.numeric(Sys.time()),
                             metadata = list()) {

  # find the username if not already supplied (may differ from account nickname)
  if (is.null(username) && length(account) > 0) {
    # default to empty
    username <- ""
    userinfo <- NULL
    try({ userInfo <- accountInfo(account, server) }, silent = TRUE)
    if (!is.null(userinfo$username))
      username <- userinfo$username
  }

  # find host information
  if (is.null(hostUrl) && length(server) > 0) {
    hostUrl <- ""
    serverinfo <- NULL
    try({ serverinfo <- serverInfo(server) }, silent = TRUE)
    if (!is.null(serverinfo$url))
      hostUrl <- serverinfo$url
  }

  # compose the standard set of fields and append any requested
  as.data.frame(c(
      list(name = name,
           title = if (is.null(title)) "" else title,
           username = username,
           account = account,
           server = server,
           hostUrl = hostUrl,
           appId = appId,
           bundleId = if (is.null(bundleId)) "" else bundleId,
           url = url,
           when = when,
           lastSyncTime = lastSyncTime),
      metadata),
    stringsAsFactors = FALSE)
}


deploymentHistoryDir <- function() {
  rsconnectConfigDir("deployments")
}

addToDeploymentHistory <- function(appPath, deploymentRecord) {

  # path to deployments files
  history <- file.path(deploymentHistoryDir(), "history.dcf")
  newHistory <- file.path(deploymentHistoryDir(), "history.new.dcf")

  # add the appPath to the deploymentRecord
  deploymentRecord$appPath <- appPath

  # write new history file
  write.dcf(deploymentRecord, newHistory, width = 4096)
  cat("\n", file = newHistory, append = TRUE)

  # append existing history to new history
  if (file.exists(history))
    file.append(newHistory, history)

  # overwrite with new history
  file.rename(newHistory, history)
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
