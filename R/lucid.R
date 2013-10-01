
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
    
    createApplication = function(name, accountId) {
      path = "/v1/applications"
      
      # appname regex:  ^[A-Za-z0-9_-]{4,63}$
      # TODO: json to file that we post
      
      # TODO: 407 means the application already exists
      
    },
    
    uploadBundle = function(file) {
      POST(authInfo, 
           "/bundle/upload", 
           "application/x-compressed", 
           file)
    }
  )
}

handleResponse <- function(response, transform = function(json) json) {
  
  # function to report errors
  reportError <- function(msg) {
    stop(paste(response$path, "-", msg), call. = FALSE)
  }
  
  # json responses
  if (isContentType(response, "application/json")) {
    
    json <- RJSONIO::fromJSON(response$content, simplify = FALSE)
    
    if (response$status %in% 200:299)
      transform(json)
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
