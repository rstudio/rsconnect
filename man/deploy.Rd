\name{deploy}
\alias{deploy}
\title{Deploy an application to ShinyApps}
\usage{
  deploy(appDir = getwd(), appName = NULL, account = NULL,
    http = getOption("shinyapps.http", "rcurl"))
}
\arguments{
  \item{appDir}{Directory containing application source
  code (defaults to current working directory)}

  \item{appName}{Name of application (names must be unique
  with ShinyApps accounts)}

  \item{account}{ShinyApps account to deploy application
  to. This parameter is only required for the initial
  deployment of an application when there are multiple
  accounts configured on the system.}

  \item{http}{Method to be used for uploading.
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

