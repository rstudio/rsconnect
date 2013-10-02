
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
     
    if (response$status %in% 200:299)
      if (!is.null(jsonFilter))
        jsonFilter(json)
      else
        json
    else if (!is.null(json$error)) {
      reportError(json$error)
    }
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
