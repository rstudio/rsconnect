\name{deploy}
\alias{deploy}
\title{Deploy an application to ShinyApps}
\usage{
  deploy(appDir = getwd(), appName = NULL, account = NULL)
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
}
\description{
  Deploy an application to ShinyApps
}

