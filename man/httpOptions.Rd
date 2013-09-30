\name{httpOptions}
\alias{httpOptions}
\title{Specify HTTP transport for communicating with ShinyApps}
\usage{
  httpOptions()
}
\description{
  Returns a character vector containing all possible values
  for the \code{shinyapps.http} user option. \code{"rcurl"}
  uses the RCurl package to create a secure https
  connection; \code{"curl"} uses the curl system utility to
  create a secure https connection; \code{"insecure"}
  creates an insecure http socket connection; The global
  default behavior can be configured by setting the
  \code{shinyapps.http} option (the default is
  \code{"rcurl"}).
}

