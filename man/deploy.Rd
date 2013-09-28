\name{deploy}
\alias{deploy}
\title{Deploy an application to ShinyApps}
\usage{
  deploy(appDir = ".",
    http = getOption("shinyapps.http", "rcurl"))
}
\arguments{
  \item{appDir}{directory of application to deploy
  (defaults to current working directory)}

  \item{http}{http method to be used for uploading.
  \code{"rcurl"} uses the RCurl package to create a secure
  https connection; \code{"curl"} uses the curl system
  utility to create a secure https connection;
  \code{"insecure"} creates an insecure http socket
  connection; The global default behavior can be configured
  by setting the \code{shinyapps.http} option (the default
  is \code{"rcurl"}).}
}
\description{
  Deploy an application to ShinyApps
}

