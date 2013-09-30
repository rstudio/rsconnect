
uploadBundle <- function(httpType, accountInfo, file) {
  httpPost(httpType, 
           accountInfo, 
           "/bundle/upload", 
           "application/x-compressed", 
           file)
} 

serviceStatus <- function(httpType, accountInfo) {
  httpGet(httpType, accountInfo,  "/internal/status")
}

getUser <- function(httpType, accountInfo, userId) {
  httpGet(httpType, accountInfo, paste("/v1/users", userId, sep="/"))
}