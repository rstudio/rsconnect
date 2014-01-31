

saveDeployment <- function(appDir, name, account, bundleId, url) {
  
  deployment <- deploymentRecord(name, account, bundleId, url)
  write.dcf(deployment, deploymentFile(appDir, name, account))
  invisible(NULL)
}

#' Read Application Deployments
#' 
#' Show deployment records for a given application directory. 
#' @param appDir The directory from which to read deployment records. 
#' @param nameFilter Return only deployments matching the given name (optional)
#' @param accountFilter Return only deployments matching the given account
#'   (optional)
#' @examples
#' \dontrun{
#' 
#' # Show all deployments of the ~/r/myapp directory made with the 'abc' account
#' readDeployments("~/r/myapp", accountFilter="abc")
#' }
#' @seealso \code{\link{applications}} to get a list of deployments from the 
#'   server, and \code{\link{deployApp}} to create a new deployment.
#' @export
readDeployments <- function(appDir, nameFilter = NULL, accountFilter = NULL) {
  
  deployments <- deploymentRecord(name = character(),
                                  account = character(),
                                  bundleId = character(),
                                  url = character())
  
  shinyappsDir <- file.path(appDir, "shinyapps")
  for (accountDir in file.path(shinyappsDir, list.files(shinyappsDir))) {
    
    # ignore regular files
    if (!file.info(accountDir)$isdir)
      next
    
    # apply optional account filter
    if (!is.null(accountFilter) && !identical(accountFilter, 
                                              basename(accountDir)))
      next
    
    deploymentFiles <- list.files(accountDir, glob2rx("*.dcf"))
    for (deploymentFile in file.path(accountDir, deploymentFiles)) {
      
      deployment <- as.data.frame(read.dcf(deploymentFile), 
                                  stringsAsFactors = FALSE)
      
      # apply optional name filter
      name <- tools::file_path_sans_ext(basename(deploymentFile))
      if (!is.null(nameFilter) && !identical(nameFilter, name)) 
        next
      
      deployments <- rbind(deployments, deployment)
    }
  }
  
  deployments
}

deploymentFile <- function(appDir, name, account) {
  accountDir <- file.path(appDir, "shinyapps", account)
  if (!file.exists(accountDir))
    dir.create(accountDir, recursive=TRUE)
  file.path(accountDir, paste(name, ".dcf", sep=""))
}

deploymentRecord <- function(name, account, bundleId, url) {
  data.frame(name = name,
             account = account,
             bundleId = bundleId,
             url = url,
             stringsAsFactors = FALSE)
}
