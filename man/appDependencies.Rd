\name{appDependencies}
\alias{appDependencies}
\title{Detect Application Dependencies}
\usage{
  appDependencies(appDir = getwd())
}
\arguments{
  \item{appDir}{Directory containing application. Defaults
  to current working directory.}
}
\value{
  Chracter vector with a list of recursive package
  dependencies for the application.
}
\description{
  Recursively detect all package dependencies for an
  application. This function parses all .R files in the
  applicaition directory to determine what packages the
  application depends on; and for each of those packages
  what other packages they depend on.
}
\details{
  Dependencies are determined by parsing application source
  code and looking for calls to \code{library},
  \code{require}, \code{::}, and \code{:::}.

  If your application has a package dependency that is not
  detected you can ensure that it is detected by inserting
  a call to \code{library} or \code{require} with the
  appropriate package(s).

  Recursive dependencies are detected by examining the
  \code{Depends}, \code{Imports}, \code{LinkingTo}, and
  \code{Suggests} fields of the packages immediately
  dependend on by the application.
}
\examples{
\dontrun{

# dependencies for the app in the current working dir
appDependencies()

# dependencies for an app in another directory
appDependencies("~/projects/shiny/app1")
}
}
\seealso{
  \link[shinyapps:shinyappsPackages]{Using Packages with
  ShinyApps}
}

