\name{publish}
\alias{publish}
\title{Publish an application to shinyapps.io}
\usage{
  publish(appDir = ".",
    method = getOption("shinyapps.upload.method", "auto"))
}
\arguments{
  \item{appDir}{directory of application to publish}

  \item{method}{Method to be used for uploading. "curl"
  uses the curl binary to do an https upload; "rcurl" uses
  the RCurl package to do an https upload; "insecure" uses
  an insecure http socket connection; and "auto" uses
  "curl" if it can find it on the system and then "rcurl"
  as a fallback. The global default behavior can be
  configured by setting the \code{shinyapps.upload.method}
  option (the default is "auto").}
}
\description{
  Publish an application to shinyapps.io
}

