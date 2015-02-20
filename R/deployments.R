

saveDeployment <- function(appPath, name, account, server, bundleId, url) {

  deployment <- deploymentRecord(name, account, server, bundleId, url)
  write.dcf(deployment, deploymentFile(appPath, name, account, server))
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
#' @return
#' Returns a data frame with the following columns:
#' \tabular{ll}{
#' \code{name} \tab Name of deployed application\cr
#' \code{account} \tab Account owning deployed application\cr
#' \code{bundleId} \tab Identifier of deployed application's bundle\cr
#' \code{url} \tab URL of deployed application\cr
#' }
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
                        serverFilter = NULL) {

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

  # build list of deployment records
  deploymentRecs <- deploymentRecord(name = character(),
                                     account = character(),
                                     server = character(),
                                     bundleId = character(),
                                     url = character())
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

    # parse file
    deployment <- as.data.frame(readDcf(file.path(rsconnectDir, deploymentFile)),
                                stringsAsFactors = FALSE)

    # fill in any missing columns and remove any extra so we can rbind
    # successfully
    missingCols <- setdiff(colnames(deploymentRecs), colnames(deployment))
    if (length(missingCols) > 0) {
      deployment[,missingCols] <- ""
    }
    extraCols <- setdiff(colnames(deployment), colnames(deploymentRecs))
    if (length(extraCols) > 0) {
      deployment[,extraCols] <- NULL
    }

    # append to record set to return
    deploymentRecs <- rbind(deploymentRecs, deployment)
  }

  deploymentRecs
}

deploymentFile <- function(appPath, name, account, server) {
  accountDir <- file.path(rsconnectRootPath(appPath), server, account)
  if (!file.exists(accountDir))
    dir.create(accountDir, recursive=TRUE)
  file.path(accountDir, paste(name, ".dcf", sep=""))
}

deploymentRecord <- function(name, account, server, bundleId, url) {
  data.frame(name = name,
             account = account,
             server = server,
             bundleId = bundleId,
             url = url,
             stringsAsFactors = FALSE)
}
