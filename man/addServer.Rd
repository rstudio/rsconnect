% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/servers.R
\name{addServer}
\alias{addServer}
\alias{removeServer}
\alias{addServerCertificate}
\title{Server management}
\usage{
addServer(
  url,
  name = NULL,
  certificate = NULL,
  validate = TRUE,
  snowflakeConnectionName = NULL,
  quiet = FALSE
)

removeServer(name = NULL)

addServerCertificate(name, certificate, quiet = FALSE)
}
\arguments{
\item{url}{URL for the server. Can be a bare hostname like
\code{connect.mycompany.com} or a url like \verb{http://posit.mycompany.com/connect}.}

\item{name}{Server name. If omitted, the server hostname is used.}

\item{certificate}{Optional. Either a path to certificate file or a
character vector containing the certificate's contents.}

\item{validate}{Validate that \code{url} actually points to a Posit Connect
server?}

\item{snowflakeConnectionName}{Name for the Snowflake connection parameters
stored in \code{connections.toml}.}

\item{quiet}{Suppress output and prompts where possible.}
}
\description{
These functions manage the list of known servers:
\itemize{
\item \code{addServer()} registers a Posit connect server. Once it has been
registered, you can connect to an account on the server using
\code{\link[=connectUser]{connectUser()}}.
\item \code{removeServer()} removes a server from the registry.
\item \code{addServerCertificate()} adds a certificate to a server.
}
}
\examples{
\dontrun{
# register a local server
addServer("http://myrsconnect/", "myserver")

# list servers
servers(local = TRUE)

# connect to an account on the server
connectUser(server = "myserver")
}
}
