
# return a list of functions that can be used to interact with lucid
lucidClient <- function(authInfo) {
  
  list(
    
    status = function() {
      handleResponse(GET(authInfo,  "/internal/status"))
    },
    
    currentUser = function() {
      handleResponse(GET(authInfo, "/users/current"))
    },
    
    accountsForUser = function(userId) {
      path <- paste("/users/", userId, "/accounts", sep="")
      listRequest(authInfo, path, "accounts")
    },
    
    applications = function(accountId) {
      path <- paste("/accounts/", accountId, "/applications", sep="")
      listRequest(authInfo, path, "applications")
    },
    
    createApplication = function(name, template, accountId) {    
      json <- list()
      json$name <- name
      json$template <- template
      json$account <- as.numeric(accountId)
      handleResponse(POST_JSON(authInfo, "/applications/", json))      
    },
    
    uploadApplication = function(applicationId, bundlePath) {
      path <- paste("/applications/", applicationId, "/upload", sep="")
      handleResponse(POST(authInfo, path, "application/x-gzip", bundlePath))
    },
    
    deployApplication = function(applicationId, bundleId) {
      path <- paste("/applications/", applicationId, "/deploy", sep="")
      json <- list()
      json$bundle <- as.numeric(bundleId)
      handleResponse(POST_JSON(authInfo, path, json))
    },
    
    terminateApplication = function(applicationId) {
      path <- paste("/applications/", applicationId, "/terminate", sep="")
      handleResponse(POST_JSON(authInfo, path, list()))
    },
    
    scaleApplication = function(applicationId, instances) {
      path <- paste("/applications/", applicationId, "/scale", sep="")
      json <- list()
      json$instance_count <- instances
      handleResponse(POST_JSON(authInfo, path, json))
    },
    
    waitForTaskCompletion = function(taskId, quiet = FALSE) {    
      
      path <- paste("/tasks/", taskId, sep="")

      if (!quiet) {
        cat("Waiting for task: ", taskId, "\n", sep="")
      }
      
      lastStatus <- NULL
      while(TRUE) {
        # check status
        status <- handleResponse(GET(authInfo, path))
        
        # display status to the user if it changed
        if (!identical(lastStatus, status$description)) {
          if (!quiet)
            cat("  ", status$status, ": ", status$description, "\n", sep="")
          lastStatus <- status$description
        }
        
        # are we finished? (note: this codepath is the only way to exit 
        # this function)
        if (status$finished) {
          if (identical(status$status, "complete"))
            return (NULL)
          else
            stop(status$error, call. = FALSE)  
        }
        
        # wait for 1 second before polling again
        Sys.sleep(1)
      }
    }
  )
}

listRequest <- function(authInfo, path, listName, pageSize = 100) {
  
  # accumulate multiple pages of results
  offset <- 0
  results <- list()
  
  while(TRUE) {
    
    # add query params
    pathWithQuery <- paste(path, "?count=", pageSize, 
                                 "&offset=", offset, 
                           sep="")
    
    # make request and append the results
    response <- handleResponse(GET(authInfo, pathWithQuery))        
    results <- append(results, response[[listName]])
    
    # update the offset
    offset <- offset + response$count
    
    # exit if we've got them all
    if (length(results) >= response$total)
      break
  }
  
  results
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
