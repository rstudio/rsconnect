##' ShinyApps
##' 
##' An \R package for deploying \pkg{shiny} applications to the ShinyApps
##' service.\cr\cr
##' Please read
##' \href{http://shiny.rstudio.com/articles/shinyapps.html}{Getting Started with ShinyApps.io}
##' for information on getting started with \pkg{shinyapps}.
##' 
##' @section Managing Applications:
##' 
##' Deploy and manage applications on the ShinyApps service.
##' 
##' \tabular{ll}{
##'   
##'   \code{\link{deployApp}:} \tab
##'   Deploy a Shiny application to the ShinyApps service.\cr\cr
##'   
##'   \code{\link{configureApp}:} \tab
##'   Configure an application currently running on ShinyApps.\cr\cr
##'   
##'   \code{\link{restartApp}:} \tab
##'   Restart an application currently running on ShinyApps.\cr\cr
##'   
##'   \code{\link{terminateApp}:} \tab
##'   Terminate an application currently running on ShinyApps.\cr\cr
##'   
##'   \code{\link{deployments}:} \tab
##'   List deployment records for a given application directory.
##'   
##' }
##' 
##' More information on application management is available in the
##' \code{\link{applications}} help page.
##' 
##' @section Managing Accounts and Users:
##' 
##' Manage ShinyApps accounts on the local system.
##
##' \tabular{ll}{
##'    
##'   \code{\link{setAccountInfo}:} \tab
##'   Register an account.\cr\cr
##'   
##'   \code{\link{removeAccount}:} \tab
##'   Remove an account.\cr\cr
##'   
##'   \code{\link{removeAuthorizedUser}:} \tab
##'   Remove authorized user from an application.\cr\cr
##'   
##'   \code{\link{accountInfo}:} \tab
##'   View information for a given account.\cr\cr
##' }
##' 
##' More information on account management is available in the
##' \code{\link{accounts}} help page.
##' 
##' @name shinyapps-package
##' @aliases shinyapps-package shinyapps
##' @docType package
##' @keywords package
NULL