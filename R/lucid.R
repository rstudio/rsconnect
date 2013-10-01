
# return a list of functions that can be used to interact with lucid
lucidClient <- function(authInfo) {
  
  list(
    
    status = function() {
      handleResponse(httpGet(authInfo,  "/internal/status"))
    },
    
    currentUser = function() {
      handleResponse(httpGet(authInfo, "/v1/users/current"))
    },
    
    accountsForUser = function(userId) {
      path <- paste("/v1/users/", userId, "/accounts", sep="")
      handleResponse(httpGet(authInfo, path), 
                     function(json) json$accounts)
    },
    
    createApplication = function(name, accountId) {
      path = "/v1/applications"
      
      # TODO: json to file that we post
      
      # TODO: 407 means the application already exists
      
    },
    
    uploadBundle = function(file) {
      httpPost(authInfo, 
               "/bundle/upload", 
               "application/x-compressed", 
               file)
    }
  )
}

handleResponse <- function(response, transform = function(json) json) {
  
  # json responses
  if (grepl("application/json", response$contentType, fixed = TRUE)) {
    
    json <- RJSONIO::fromJSON(response$content, simplify = FALSE)
    
    if (response$status %in% 200:299)
      return (transform(json))
    else if (!is.null(json$error)) {
      stop(paste(response$path, "-", json$error), call. = FALSE)
    }
  }
  
  # for html responses we can attempt to extract the body 
  stop(paste(response$path, "-", response$content), call. = FALSE)
}


