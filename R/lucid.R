
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
                     jsonFilter = function(json) json$accounts)
    },
    
    applications = function() {
      handleResponse(GET(authInfo, "/v1/applications/"))
    },
    
    createApplication = function(name, accountId, existingOK = FALSE) {
      
      if (existingOK)
        statusFilter <-function(status) status == 409
      else
        statusFilter <- NULL
      
      path = "/v1/applications/"
      json <- list()
      json$name <- name
      json$template <- "shiny"
      json$account <- as.numeric(accountId)
      handleResponse(POST_JSON(authInfo, path, json),
                     statusFilter = statusFilter)      
    },
    
    uploadBundle = function(file) {
      POST(authInfo, 
           "/bundle/upload", 
           "application/x-compressed", 
           file)
    }
  )
}

handleResponse <- function(response, 
                           statusFilter = NULL,
                           jsonFilter = NULL) {
  
  # function to report errors
  reportError <- function(msg) {
    stop(paste(response$path, response$status, "-", msg), call. = FALSE)
  }
  
  # json responses
  if (isContentType(response, "application/json")) {
    
    json <- RJSONIO::fromJSON(response$content, simplify = FALSE)
    
    isSuccess <- function(status) {
      if (response$status %in% 200:299)
        TRUE
      else if (!is.null(statusFilter) && statusFilter(status))
        TRUE
      else
        FALSE
    }
    
    if (isSuccess(response$status))
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
