
#' List Deployed Applications
#' 
#' List all applications currently deployed to ShinyApps for a given account.
#' @param account Account name. If a single account is registered on the 
#' system then this parameter can be omitted.
#' @return 
#' Returns a data frame with the following columns:
#' \tabular{ll}{
#' \code{id} \tab Unique id for the application\cr
#' \code{name} \tab Name of application \cr 
#' \code{url} \tab URL where application can be accessed\cr
#' \code{status} \tab Current status of application. Valid values are \code{pending},
#' \code{deploying}, \code{running}, \code{terminating}, \code{terminated}.
#' }
#' @note To register an account you call the \link{setAccountInfo} function.
#' @examples
#' \dontrun{
#' 
#' # list all applications for the default account
#' applications()
#' 
#' # list all applications for a specific account
#' applications("myaccount")
#' }
#' @export
applications <- function(account = NULL) {
  
  # resolve account (use default if possible, confirm exists, etc.)
  accountInfo <- accountInfo(resolveAccount(account))
  
  # create lucid client and retreive applications
  lucid <- lucidClient(accountInfo)
  apps <- lucid$applications(accountInfo$accountId)
  
  # convert the list into a data frame with a subset of fields
  id <- numeric()
  name <- character()
  url <- character()
  status <- character()
  for (app in apps) {
    id <- append(id, app$id)
    name <- append(name, app$name)
    url <- append(url, app$url)
    status <- append(status, app$status)
  }
  data.frame(id = id,
             name = I(name),
             url = I(url),
             status = status)
}
