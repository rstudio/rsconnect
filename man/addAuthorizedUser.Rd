\name{addAuthorizedUser}
\alias{addAuthorizedUser}
\title{Add authorized user for application}
\usage{
addAuthorizedUser(username, password = NULL, appDir = getwd())
}
\arguments{
  \item{username}{}

  \item{password}{}

  \item{appDir}{Directory containing application. Defaults
  to current working directory.}
}
\description{
Add authorized user for application
}
\examples{
\dontrun{

# add a user (prompts for password)
addAuthroizedUser("andy")

# add a user using supplied password
addAuthorizedUser("tareef", "MrShiny45")

}
}
\seealso{
\code{\link{removeAuthorizedUser}} and
\code{\link{authorizedUsers}}
}

