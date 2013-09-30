
# return a list of functions that can be used to interact with lucid
lucidClient <- function(authInfo) {
  
  list(
    uploadBundle = function(file) {
      httpPost(authInfo, 
               "/bundle/upload", 
               "application/x-compressed", 
               file)
    }, 
    
    status = function() {
      httpGet(authInfo,  "/internal/status")
    },
    
    getUser = function(userId) {
      httpGet(authInfo, paste("/v1/users", userId, sep="/"))
    }
  )
}



