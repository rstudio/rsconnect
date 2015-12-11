

saveDeployment <- function(appPath, name, account, server, appId, bundleId, url,
                           metadata) {

  # create the record to write to disk
  deployment <- deploymentRecord(name, account, server, appId, bundleId, url,
                                 when = as.numeric(Sys.time()),
                                 metadata)

  # use a long width so URLs don't line-wrap
  write.dcf(deployment, deploymentFile(appPath, name, account, server),
            width = 4096)

  # also save to global history
  addToDeploymentHistory(appPath, deployment)

  invisible(NULL)
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
#' @param excludeOrphaned If \code{TRUE} (the default), return only deployments
#'   made by a currently registered account. Deployments made from accounts that
#'   are no longer registered (via e.g.\code{\link{removeAccount}}) will not be
#'   returned.
#' @return
#' Returns a data frame with at least following columns:
#' \tabular{ll}{
#' \code{name} \tab Name of deployed application\cr
#' \code{account} \tab Account owning deployed application\cr
#' \code{bundleId} \tab Identifier of deployed application's bundle\cr
#' \code{url} \tab URL of deployed application\cr
#' \code{when} \tab When the application was deployed (in seconds since the
#'   epoch)\cr
#' }
#'
#' If additional metadata has been saved with the deployment record using the
#' \code{metadata} argument to \code{\link{deployApp}}, the frame will include
#' additional columns.
#'
#' @examples
#' \dontrun{
#'
#' # Return all deployments of the ~/r/myapp directory made with the 'abc'
#' # account
#' deployments("~/r/myapp", accountFilter="abc")
#' }
#' @seealso \code{\link{applications}} to get a list of deployments from the
#'   server, and \code{\link{deployApp}} to create a new deployment.
#' @export
deployments <- function(appPath, nameFilter = NULL, accountFilter = NULL,
                        serverFilter = NULL, excludeOrphaned = TRUE) {

  # calculate rsconnect dir
  rsconnectDir <- rsconnectRootPath(appPath)

  # calculate migration dir--all shinyapps deployment records go into the root
  # folder since it wasn't possible to deploy individual docs using the
  # shinyapps package
  migrateRoot <- if (isDocumentPath(appPath)) dirname(appPath) else appPath

  # migrate shinyapps package created records if necessary
  shinyappsDir <- file.path(migrateRoot, "shinyapps")
  if (file.exists(shinyappsDir)) {
    migrateDir <- file.path(migrateRoot, "rsconnect")
    for (shinyappsFile in list.files(shinyappsDir, glob2rx("*.dcf"),
                                     recursive = TRUE)) {
      # read deployment record
      shinyappsDCF <- file.path(shinyappsDir, shinyappsFile)
      deployment <- as.data.frame(readDcf(shinyappsDCF),
                                  stringsAsFactors = FALSE)
      deployment$server <- "shinyapps.io"

      # write the new record
      rsconnectDCF <- file.path(migrateDir, "shinyapps.io", shinyappsFile)
      dir.create(dirname(rsconnectDCF), showWarnings = FALSE, recursive = TRUE)
      write.dcf(deployment, rsconnectDCF)

      # remove old DCF
      file.remove(shinyappsDCF)
    }

    # remove shinyapps dir if it's completely empty
    remainingFiles <- list.files(shinyappsDir,
                                 recursive = TRUE,
                                 all.files = TRUE)
    if (length(remainingFiles) == 0)
      unlink(shinyappsDir, recursive = TRUE)
  }

  # get list of active accounts
  activeAccounts <- accounts()

  # build list of deployment records
  deploymentRecs <- deploymentRecord(name = character(),
                                     account = character(),
                                     server = character(),
                                     appId = character(),
                                     bundleId = character(),
                                     url = character(),
                                     when = numeric())
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
    name <- tools::file_path_sans_ext(basename(deploymentFile))
    if (!is.null(nameFilter) && !identical(nameFilter, name))
      next

    # exclude orphaned if requested (note that the virtual server "rpubs.com"
    # is always considered to be registered)
    if (excludeOrphaned && server != "rpubs.com") {
      # filter by account name and then by server
      matchingAccounts <- activeAccounts[activeAccounts[["name"]] == account,]
      matchingAccounts <-
        matchingAccounts[matchingAccounts[["server"]] == server,]

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
      deployment[,missingCols] <- NA
    }

    # if this record contains any columns that aren't present everywhere, add
    # them
    extraCols <- setdiff(colnames(deployment), colnames(deploymentRecs))
    if (length(extraCols) > 0 && nrow(deploymentRecs) > 0) {
      deploymentRecs[,extraCols] <- NA
    }

    # append to record set to return
    deploymentRecs <- rbind(deploymentRecs, deployment)
  }

  deploymentRecs
}

deploymentFile <- function(appPath, name, account, server) {
  accountDir <- file.path(rsconnectRootPath(appPath), server, account)
  if (!file.exists(accountDir))
    dir.create(accountDir, recursive = TRUE)
  file.path(accountDir, paste0(name, ".dcf"))
}

deploymentRecord <- function(name, account, server, appId, bundleId, url, when,
                             metadata = list()) {
  # compose the standard set of fields and append any requested
  as.data.frame(c(
      list(name = name,
           account = account,
           server = server,
           appId = appId,
           bundleId = bundleId,
           url = url,
           when = when),
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




