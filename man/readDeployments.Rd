\name{readDeployments}
\alias{readDeployments}
\title{Read Application Deployments}
\usage{
readDeployments(appDir, nameFilter = NULL, accountFilter = NULL)
}
\arguments{
  \item{appDir}{The directory from which to read deployment
  records.}

  \item{nameFilter}{Return only deployments matching the
  given name (optional)}

  \item{accountFilter}{Return only deployments matching the
  given account (optional)}
}
\description{
Show deployment records for a given application directory.
}
\examples{
\dontrun{

# Show all deployments of the ~/r/myapp directory made with the 'abc' account
readDeployments("~/r/myapp", accountFilter="abc")
}
}
\seealso{
\code{\link{applications}} to get a list of deployments
from the server, and \code{\link{deployApp}} to create a
new deployment.
}

