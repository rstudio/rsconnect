\name{deploy}
\alias{deploy}
\title{Deploy an application to ShinyApps}
\usage{
  deploy(appDir = getwd(), appName = NULL, account = NULL,
    quiet = FALSE)
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

  \item{quiet}{Request that no status information be
  printed to the console during the deployment.}
}
\description{
  Deploy an application to ShinyApps
}

