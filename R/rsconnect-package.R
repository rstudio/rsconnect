#' Deployment Interface for R Markdown Documents and Shiny Applications
#'
#' The `rsconnect`` package provides a programmatic deployment
#' interface for RPubs, shinyapps.io, and Posit Connect. Supported contents
#' types include R Markdown documents, Shiny applications, plots, and static
#' web content.
#'
#' @section Managing Applications:
#'
#' Deploy and manage applications with the following functions:
#'
#' * [deployApp()]:
#' Deploy a Shiny application to a server.
#'
#' * [configureApp()]:
#' Configure an application currently running on a server.
#'
#' * [restartApp()]:
#' Restart an application currently running on a server.
#'
#' * [terminateApp()]:
#' Terminate an application currently running on a server.
#'
#' * [deployments()]:
#' List deployment records for a given application directory.
#'
#'
#' More information on application management is available in the
#' [applications()] help page.
#'
#' @section Managing Accounts and Users:
#'
#' Manage accounts on the local system.
##
#' * [setAccountInfo()]:
#'   Register an account.
#'
#' * [removeAccount()]:
#'   Remove an account.
#'
#' * [accountInfo()]:
#'   View information for a given account.
#'
#' More information on account management is available in the
#' [accounts()] help page.
#'
#' @name rsconnect-package
#' @aliases rsconnect-package rsconnect
#' @docType package
#' @keywords package
NULL
