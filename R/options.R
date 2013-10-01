

#' Specify HTTP transport for communicating with ShinyApps
#' 
#' Returns a character vector containing all possible values for the 
#' \code{shinyapps.http} user option. \code{"curl"} uses the curl system utility 
#' to create a secure https connection; \code{"rcurl"} uses the RCurl package to 
#' create a secure https connection; \code{"insecure"} creates an insecure 
#' http socket connection; \code{"auto"} uses \code{"curl"} if available and
#' otherwise uses \code{"rcurl"}  The global default behavior can be configured
#' by setting the \code{shinyapps.http} option (the default is \code{"auto"}).
#' @export
httpOptions <- function() {
  c("curl", "rcurl", "insecure", "auto")
}
