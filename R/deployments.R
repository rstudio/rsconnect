

saveDeployment <- function(appDir, name, account, server, bundleId, url) {

  deployment <- deploymentRecord(name, account, server, bundleId, url)
  write.dcf(deployment, deploymentFile(appDir, name, account, server))
  invisible(NULL)
}

#' List Application Deployments
#'
#' List deployment records for a given application directory.
#' @param appDir The directory from which to read deployment records.
#' @param nameFilter Return only deployments matching the given name (optional)
#' @param accountFilter Return only deployments matching the given account
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
deployments <- function(appDir, nameFilter = NULL, accountFilter = NULL,
                        serverFilter = NULL) {

  deploymentRecs <- deploymentRecord(name = character(),
                                     account = character(),
                                     server = character(),
                                     bundleId = character(),
                                     url = character())

  rsconnectDir <- file.path(appDir, "rsconnect")
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
    deploymentRecs <- rbind(deploymentRecs, deployment)
  }

  deploymentRecs
}

deploymentFile <- function(appDir, name, account, server) {
  accountDir <- file.path(appDir, "rsconnect", server, account)
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
