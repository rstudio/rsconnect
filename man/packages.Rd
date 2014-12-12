\name{rsconnectPackages}
\alias{rsconnectPackages}

\title{Using Packages with rsconnect}

\description{
Deployed applications can depend on any package available on \href{http://cran.rstudio.com/}{CRAN} as well as any package hosted in a public \href{https://www.github.com}{GitHub} repository.

When an application is deployed it's source code is scanned for dependencies using the \code{\link{appDependencies}} function. The list of dependencies is sent to the server along with the application source code and these dependencies are then installed alongside the application.

Note that the \code{Suggests} dependencies of packages are not automatically included in the list of dependent packages. See the \emph{Note} section of the documentation of the \link{appDependencies} function for details on how to force packages to be included in the dependency list.
}

\section{CRAN Packages}{
When sastisfying CRAN package dependencies, the server will build the exact versions of packages that were installed on the system from which the application is deployed.

If a locally installed package was not obtained from CRAN (e.g. was installed from R-Forge) and as a result doesn't have a version that matches a version previously published to CRAN then an error will occur. It's therefore important that you run against packages installed directly from CRAN in your local configuration.
}

\section{GitHub Packages}{
It's also possible to depend on packages hosted in public GitHub repositories, so long as they are installed via the \link[devtools:install_github]{install_github} function from the \pkg{devtools} package.

This works because \code{install_github} records the exact Github commit that was installed locally, making it possible to download and install the same source code on the deployment server.

Note that in order for this to work correctly you need to install the very latest version of \code{devtools} from Github. You can do this as follows:

\preformatted{
library(devtools)
install_github("devtools", "hadley")
}
}

\seealso{
\code{\link{appDependencies}}
}
