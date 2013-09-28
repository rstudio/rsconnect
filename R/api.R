
uploadBundle <- function(http, file) {
  httpPost(http, "/bundle/upload", "application/x-compressed", file)
} 

serviceStatus <- function(http) {
  httpGet(http, "/internal/status")
}
