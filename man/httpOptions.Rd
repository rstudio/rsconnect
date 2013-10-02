\name{httpOptions}
\alias{httpOptions}
\title{HTTP transport for communicating with ShinyApps}
\usage{
  httpOptions()
}
\description{
  Returns a character vector containing all possible values
  for the \code{shinyapps.http} user option. \code{"rcurl"}
  uses the RCurl package to create a secure https
  connection; \code{"curl"} uses the curl system utility to
  create a secure https connection; \code{"internal"}
  creates an insecure http socket connection. The default
  when no option is specified is \code{"rcurl"}.
}

