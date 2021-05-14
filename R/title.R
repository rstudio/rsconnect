#' Generate Application Name
#'
#' Generate a short name (identifier) for an application given an application
#' title.
#'
#' @param appTitle A descriptive title for the application.
#' @param appPath The path to the application's content, either a directory
#'   or an individual document. Optional.
#' @param account The account where the application will be deployed. Optional.
#' @param unique Whether to try to generate a unique name.
#' @return
#' Returns a valid short name for the application.
#'
#' @details
#' This function modifies the title until it forms a suitable application name.
#' Suitable application names are 3 - 64 characters long and contain only
#' alphanumeric characters.
#'
#' The function is intended to be used to find a name for a new application.
#' If `appPath` and `account` are both specified, then the returned
#' name will also be unique among locally known deployments of the directory
#' (note that it is not guaranteed to be unique on the server). This behavior
#' can be disabled by setting `unique = FALSE`.
#'
#' @examples
#' \dontrun{
#' # Generate a short name for a sample application
#' generateAppName("My Father's Country", "~/fathers-country", "myacct")
#' }
#' @export

generateAppName <- function(appTitle, appPath = NULL, account = NULL, unique = TRUE) {
  munge <- function (title) {
    # safe default if no title specified
    if (is.null(title)) {
      return("")
    }

    # start by removing most non-Latin characters and converting to lowercase
    name <- tolower(gsub("[^A-Za-z0-9_ -]+", "", title))

    # replace spaces with underscores
    name <- gsub(" ", "_", name, fixed = TRUE)

    # trim to 64 characters
    if (nchar(name) > 64) {
      name <- substr(name, 1, 64)
    }

    name
  }

  name <- munge(appTitle)

  # if we wound up with too few characters, try generating from the directory
  # name instead
  if (nchar(name) < 3 && !is.null(appPath) && file.exists(appPath)) {
    # strip extension if present
    base <- basename(appPath)
    if (nzchar(tools::file_ext(base))) {
      base <- file_path_sans_ext(base)

      # if we stripped an extension and the name is now "index", use the parent
      # folder's name
      if (identical(base, "index")) {
        base <- basename(dirname(appPath))
      }
    }
    name <- munge(base)
  }

  # validate that we wound up with a valid name
  if (nchar(name) < 3) {
    stop("The generated app name '", name, "' is invalid. Include at least 3 ",
         "alphanumeric characters in the title.")
  }

  # if we have an account and a directory, make the new app name unique to the
  # best of our local knowledge
  if (unique && !is.null(appPath) && nzchar(appPath) && !is.null(account)) {
    apps <- deployments(appPath, accountFilter = account)
    if (name %in% apps$name) {
      # trim a few characters if necessary so we can add unique numbers to the end
      base <- substr(name, 1, 62)
      suffix <- 2
      candidate <- paste0(base, suffix)

      # keep incrementing the suffix until we find a unique name
      while (candidate %in% apps$name) {
         suffix <- suffix + 1
         candidate <- paste0(base, suffix)
      }
      name <- candidate
    }
  }

  name
}
