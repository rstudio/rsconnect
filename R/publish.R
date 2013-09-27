
#' Publish an application to shinyapps.io
#' 
#' Publish an application to shinyapps.io
#' @param appDir directory of application to publish
#' @param http http method to be used for uploading. "rcurl" uses the RCurl 
#'   package to create a secure https connection; "curl" uses the curl binary to
#'   create a secure https connection; "insecure" creates an insecure http raw
#'   socket connection; The global default behavior can be configured by setting
#'   the \code{shinyapps.http} option (the default is "rcurl").
#' @export
publish <- function(appDir = ".", 
                    http = getOption("shinyapps.http", "rcurl")) {
  
  
}



