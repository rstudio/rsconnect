
# return a list of functions that can be used to interact with lucid
lucidClient <- function(authInfo) {
  
  list(
    
    status = function() {
      handleResponse(GET(authInfo,  "/internal/status"))
    },
    
    currentUser = function() {
      handleResponse(GET(authInfo, "/v1/users/current"))
    },
    
    accountsForUser = function(userId) {
      path <- paste("/v1/users/", userId, "/accounts", sep="")
      handleResponse(GET(authInfo, path), 
                     function(json) json$accounts)
    },
    
    applications = function(accountId) {
      path <- paste("/v1/accounts/", accountId, "/applications", sep="")
      handleResponse(GET(authInfo, path),
                     function(json) json$applications)
    },
    
    createApplication = function(name, template, accountId) {    
      json <- list()
      json$name <- name
      json$template <- template
      json$account <- as.numeric(accountId)
      handleResponse(POST_JSON(authInfo, "/v1/applications/", json))      
    },
    
    uploadApplication = function(applicationId, bundlePath) {
      path <- paste("/v1/applications/", applicationId, "/upload", sep="")
      handleResponse(POST(authInfo, path, "application/x-gzip", bundlePath))
    },
    
    deployApplication = function(applicationId, bundleId) {
      path <- paste("/v1/applications/", applicationId, "/deploy", sep="")
      json <- list()
      json$bundle <- as.numeric(bundleId)
      handleResponse(POST_JSON(authInfo, path, json))
    },
    
    waitForTaskCompletion = function(taskId, quiet = FALSE) {    
      
      path <- paste("/v1/tasks/", taskId, sep="")
  
      lastStatus <- NULL
      while(TRUE) {
        # check status
        status <- handleResponse(GET(authInfo, path))
        
        # are we finished? (note: this codepath is the only way to exit 
        # this function)
        if (status$finished) {
          if (identical(status$status, "complete"))
            return (NULL)
          else
            stop(status$error, call. = FALSE)  
        }
        
        # display status to the user if it changed
        else if (!identical(lastStatus, status$status)) {
          if (!quiet)
            cat("  ", status$status, "\n", sep="")
          lastStatus <- status$status
        }
        
        # wait for 1 second before polling again
        Sys.sleep(1)
      }
    }
  )
}

handleResponse <- function(response, jsonFilter = NULL) {
  
  # function to report errors
  reportError <- function(msg) {
    stop(paste(response$path, response$status, "-", msg), call. = FALSE)
  }
  
  # json responses
  if (isContentType(response, "application/json")) {
    
    json <- RJSONIO::fromJSON(response$content, simplify = FALSE)
     
    if (response$status %in% 200:399)
      if (!is.null(jsonFilter))
        jsonFilter(json)
      else
        json
    else if (!is.null(json$error))
      reportError(json$error)
    else
      reportError(paste("Unexpected json response:", response$content))
  }
  
  # for html responses we can attempt to extract the body
  else if (isContentType(response, "text/html")) {
    
    body <- regexExtract(".*?<body>(.*?)</body>.*", response$content)
    if (!is.null(body))
      reportError(body)
    else
      reportError(response$content)  
  }
  
  # otherwise just dump the whole thing
  else {
    reportError(response$content)
  }
}

isContentType <- function(response, contentType) {
  grepl(contentType, response$contentType, fixed = TRUE)
}
