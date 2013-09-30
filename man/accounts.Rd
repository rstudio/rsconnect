\name{accounts}
\alias{accountInfo}
\alias{accounts}
\alias{removeAccount}
\alias{setAccountInfo}
\title{Account Management Functions}
\usage{
  accounts()

  setAccountInfo(name, key, secret)

  accountInfo(name)

  removeAccount(name)
}
\arguments{
  \item{name}{Account name}

  \item{key}{Account key}

  \item{secret}{Account secret}
}
\description{
  Functions to add, enumerate, and remove ShinyApps
  accounts on the local system.
}

