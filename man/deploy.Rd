\name{deploy}
\alias{deploy}
\title{Deploy Applications to ShinyApps}
\usage{
  deploy(appDir = getwd(), appName = NULL, account = NULL,
    launch.browser = getOption("shinyapps.launch.browser", interactive()),
    quiet = FALSE)
}
\arguments{
  \item{appDir}{Directory containing application source
  code. Defaults to current working directory.}

  \item{appName}{Name of application (names must be unique
  with ShinyApps accounts). Defaults to the base name of
  the specified \code{appDir}.}

  \item{account}{ShinyApps account to deploy application
  to. This parameter is only required for the initial
  deployment of an application when there are multiple
  accounts configured on the system (see \link{accounts}).}

  \item{launch.browser}{If true, the system's default web
  browser will be launched automatically after the app is
  started. Defaults to \code{TRUE} in interactive sessions
  only.}

  \item{quiet}{Request that no status information be
  printed to the console during the deployment.}
}
\description{
  Deploy a \link[shiny:shiny-package]{shiny} application to
  the ShinyApps service.
}
\details{
  Prior to deploying an application you should call the
  \code{\link{setAccountInfo}} function to register your
  ShinyApps account on the local system.

  After the initial deployment of an application from a
  given \code{appDir}, subsequent deployments will
  automatically use the \code{appName} and \code{account}
  parameters of the initial deployment (unless overriden
  explicitly).
}
\examples{
\dontrun{

# deploy the application in the current working dir
deploy()

# deploy an application in another directory
deploy("~/projects/shiny/app1")

# deploy using an alternative application name
deploy("~/projects/shiny/app1", appName = "myapp")

# deploy specifying an explicit account name, then
# redeploy with no arguments (will automatically use
# the previously specified account)
deploy(account = "jsmith")
deploy()

# deploy but don't launch a browser when completed
deploy(launch.browser = FALSE)
}
}

