\name{accounts}
\alias{accountInfo}
\alias{accounts}
\alias{removeAccount}
\alias{setAccountInfo}
\title{Account Management Functions}
\usage{
  accounts()

  setAccountInfo(name, token, secret)

  accountInfo(name)

  removeAccount(name)
}
\arguments{
  \item{name}{Account name}

  \item{token}{User token}

  \item{secret}{User token secret}
}
\description{
  Functions to add, enumerate, and remove ShinyApps
  accounts on the local system.
}

