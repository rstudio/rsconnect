
# return a list of functions that can be used to interact with lucid
lucidClient <- function(accountInfo) {
  
  list(
    uploadBundle = function(file) {
      httpPost(accountInfo, 
               "/bundle/upload", 
               "application/x-compressed", 
               file)
    }, 
    
    status = function() {
      httpGet(accountInfo,  "/internal/status")
    },
    
    getUser = function(userId) {
      httpGet(accountInfo, paste("/v1/users", userId, sep="/"))
    }
  )
}



