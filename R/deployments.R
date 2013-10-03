

saveDeployment <- function(appDir, name, account, bundleId, url) {
  
  deployment <- deploymentRecord(name, account, bundleId, url)
  write.dcf(deployment, deploymentFile(appDir, name, account))
  invisible(NULL)
}

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
