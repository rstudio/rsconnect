
# return a list of functions that can be used to interact with lucid
lucidClient <- function(authInfo) {
  
  list(
    
    status = function() {
      httpGet(authInfo,  "/internal/status")
    },
    
    userIdFromToken = function(token) {
      # TODO: use api once it's available
      4
    },
    
    accountsForUser = function(userId) {
      path <- paste("/v1/users/", userId, "/accounts", sep="")
      results <- httpGet(authInfo, path)
      if (results$status == 200)
        RJSONIO::fromJSON(results$content)$accounts
      else {
        # TODO: interpret error messages
        stop("Unexpected HTTP error (status = ", results$status, ")")
      }
    },
    
    uploadBundle = function(file) {
      httpPost(authInfo, 
               "/bundle/upload", 
               "application/x-compressed", 
               file)
    }, 
    
   
    
    getUser = function(userId) {
      httpGet(authInfo, paste("/v1/users", userId, sep="/"))
    }
  )
}



