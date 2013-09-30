
# return a list of functions that can be used to interact with lucid
lucidClient <- function(httpType, accountInfo) {
  
  list(
    uploadBundle = function(file) {
      httpPost(httpType, 
               accountInfo, 
               "/bundle/upload", 
               "application/x-compressed", 
               file)
    }, 
    
    status = function() {
      httpGet(httpType, accountInfo,  "/internal/status")
    },
    
    getUser = function(userId) {
      httpGet(httpType, accountInfo, paste("/v1/users", userId, sep="/"))
    }
  )
}



