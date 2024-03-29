% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{resendInvitation}
\alias{resendInvitation}
\title{Resend invitation for invited users of an application}
\usage{
resendInvitation(
  invite,
  regenerate = FALSE,
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL
)
}
\arguments{
\item{invite}{The invitation to resend. Can be id or email address.}

\item{regenerate}{Regenerate the invite code. Can be helpful is the
invitation has expired.}

\item{appDir}{Directory containing application. Defaults to
current working directory.}

\item{appName}{Name of application.}

\item{account, server}{Uniquely identify a remote server with either your
user \code{account}, the \code{server} name, or both. If neither are supplied, and
there are multiple options, you'll be prompted to pick one.

Use \code{\link[=accounts]{accounts()}} to see the full list of available options.}
}
\description{
Resend invitation for invited users of an application
}
\note{
This function works only for ShinyApps servers.
}
\seealso{
\code{\link[=showInvited]{showInvited()}}
}
