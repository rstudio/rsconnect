# Signals an error for a user record that has neither an id nor an email,
# because the record cannot identify the user.
checkCollaboratorRecord <- function(client, id, email, call = caller_env()) {
  if (is.na(id) && is.na(email)) {
    cli::cli_abort(
      c(
        "Unexpected response from {serverDisplayName(client)}: a user record has neither an {.field id} nor an {.field email}.",
        i = "The response shape may have changed; contact Posit support if this persists."
      ),
      call = call
    )
  }
}

emptyInvitations <- function() {
  data.frame(
    id = character(),
    email = character(),
    link = character(),
    expired = logical(),
    stringsAsFactors = FALSE
  )
}

abortUserManagementUnsupported <- function(client) {
  cli::cli_abort(
    "rsconnect can't manage application users on {serverDisplayName(client)}.",
    call = NULL
  )
}

cleanupPasswordFile <- function(appDir) {
  check_directory(appDir)
  appDir <- normalizePath(appDir)

  # get data dir from appDir
  dataDir <- file.path(appDir, "shinyapps")

  # get password file
  passwordFile <- file.path(dataDir, paste("passwords", ".txt", sep = ""))

  # check if password file exists
  if (file.exists(passwordFile)) {
    message(
      "WARNING: Password file found! This application is configured to use scrypt ",
      "authentication, which is no longer supported.\nIf you choose to proceed, ",
      "all existing users of this application will be removed, ",
      "and will NOT be recoverable.\nFor for more information please visit: ",
      "http://shiny.rstudio.com/articles/migration.html"
    )
    response <- readline("Do you want to proceed? [Y/n]: ")
    if (tolower(substring(response, 1, 1)) != "y") {
      stop("Cancelled", call. = FALSE)
    } else {
      # remove old password file
      file.remove(passwordFile)
    }
  }

  invisible(TRUE)
}

#' Add authorized user to application
#'
#' @description
#' Add authorized user to application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param email Email address of user to add.
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application.
#' @param contentId On Posit Connect Cloud, the content ID to manage, taken from
#'   the content URL
#'   (\code{https://connect.posit.cloud/{account}/content/{contentId}}). When
#'   supplied, \code{appDir} and \code{appName} are ignored and no local
#'   deployment record is required. Not supported on shinyapps.io.
#' @inheritParams deployApp
#' @param sendEmail Send an email letting the user know the application
#'   has been shared with them.
#' @param emailMessage Optional character vector of length 1 containing a
#'   custom message to send in email invitation. Defaults to NULL, which
#'   will use default invitation message.
#' @seealso [removeAuthorizedUser()] and [showUsers()]
#' @note This function works for ShinyApps and Posit Connect Cloud. On Posit
#'   Connect Cloud, the content's account must be an organization account.
#'   The \code{sendEmail} argument is ignored on Posit Connect Cloud;
#'   PCC always sends an invitation email.
#'
#'   On Posit Connect Cloud, the content is resolved from the local deployment
#'   record under \code{appDir}, which defaults to the working directory. Pass
#'   \code{appDir} to point at the project directory that contains the
#'   \code{rsconnect/} deployment record. \code{appName} selects among multiple records in
#'   the same directory. Alternatively, pass \code{contentId} to target the
#'   content directly, without a local deployment record.
#' @export
addAuthorizedUser <- function(
  email,
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
  account = NULL,
  server = NULL,
  sendEmail = NULL,
  emailMessage = NULL
) {
  accountDetails <- accountInfo(account, server)
  api <- clientForAccount(accountDetails)

  application <- resolveContentTarget(
    api,
    accountDetails,
    appDir,
    appName,
    contentId
  )

  if (usesPasswordFile(api)) {
    cleanupPasswordFile(appDir)
  }

  # Warn only when the caller explicitly opts out of the email.
  if (!supportsOptionalInviteEmail(api) && identical(sendEmail, FALSE)) {
    cli::cli_warn(
      "{.arg sendEmail} is ignored on {serverDisplayName(api)}, which always sends an invitation email."
    )
  }

  # fetch authorization list
  inviteApplicationUser(
    api,
    application$id,
    validateEmail(email),
    sendEmail,
    emailMessage
  )

  message(paste("Added:", email, "to application", sep = " "))

  invisible(TRUE)
}

#' Remove authorized user from an application
#'
#' @description
#' Remove authorized user from an application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param user The user to remove. Can be id or email address.
#' @param appDir Directory containing application. Defaults to
#' current working directory.
#' @param appName Name of application.
#' @param contentId On Posit Connect Cloud, the content ID to manage, taken from
#'   the content URL
#'   (\code{https://connect.posit.cloud/{account}/content/{contentId}}). When
#'   supplied, \code{appDir} and \code{appName} are ignored and no local
#'   deployment record is required. Not supported on shinyapps.io.
#' @inheritParams deployApp
#' @seealso [addAuthorizedUser()] and [showUsers()]
#' @note This function works for ShinyApps and Posit Connect Cloud.
#'
#'   On Posit Connect Cloud, the content is resolved from the local deployment
#'   record under \code{appDir}, which defaults to the working directory. Pass
#'   \code{appDir} to point at the project directory that contains the
#'   \code{rsconnect/} deployment record. \code{appName} selects among multiple records in
#'   the same directory. Alternatively, pass \code{contentId} to target the
#'   content directly, without a local deployment record.
#' @export
removeAuthorizedUser <- function(
  user,
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
  account = NULL,
  server = NULL
) {
  accountDetails <- accountInfo(account, server)
  api <- clientForAccount(accountDetails)

  application <- resolveContentTarget(
    api,
    accountDetails,
    appDir,
    appName,
    contentId
  )

  if (usesPasswordFile(api)) {
    cleanupPasswordFile(appDir)
  }

  users <- listCollaborators(api, application$id)

  user <- as.character(user)
  # Match id first (UUID strings on PCC, numeric-as-character on shinyapps.io),
  # then fall back to email. The old is.numeric() branch missed PCC UUID ids.
  if (user %in% users$id) {
    user <- users[which(users$id == user), ]
  } else if (user %in% users$email) {
    user <- users[which(users$email == user), ]
  } else {
    # The hint only helps someone who searched by email. A lookup by id is not
    # affected by redaction.
    redactionHint <- redactsUserEmails(api) && grepl("@", user, fixed = TRUE)
    cli::cli_abort(c(
      "User {.val {user}} not found.",
      i = if (redactionHint) {
        "On {serverDisplayName(api)} an email can be redacted and won't match; pass the user id from {.fn showUsers} instead."
      }
    ))
  }

  if (is.na(user$id)) {
    cli::cli_abort(c(
      "Cannot remove user {.val {user$email}}: the matched record has no id.",
      i = "This is unexpected; contact Posit support if this persists."
    ))
  }

  # remove user (api already built above)
  removeApplicationUser(api, application$id, user$id)

  message(paste("Removed:", user$email, "from application", sep = " "))

  invisible(TRUE)
}

#' List authorized users for an application
#'
#' @description
#' List authorized users for an application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application.
#' @param contentId On Posit Connect Cloud, the content ID to manage, taken from
#'   the content URL
#'   (\code{https://connect.posit.cloud/{account}/content/{contentId}}). When
#'   supplied, \code{appDir} and \code{appName} are ignored and no local
#'   deployment record is required. Not supported on shinyapps.io.
#' @inheritParams deployApp
#' @seealso [addAuthorizedUser()] and [showInvited()]
#' @return A data frame with one row per authorized user. Columns always
#'   present: \code{id}, \code{email}, \code{account}. The \code{account}
#'   column is populated on shinyapps.io only and is \code{NA} on Posit Connect
#'   Cloud. On Posit Connect Cloud the data frame additionally includes
#'   \code{display_name} and \code{role} (e.g. \code{"viewer"} or
#'   \code{"collaborator"}) from the API response.
#' @note This function works for ShinyApps and Posit Connect Cloud.
#'
#'   On Posit Connect Cloud, the content is resolved from the local deployment
#'   record under \code{appDir}, which defaults to the working directory. Pass
#'   \code{appDir} to point at the project directory that contains the
#'   \code{rsconnect/} deployment record. \code{appName} selects among multiple records in
#'   the same directory. Alternatively, pass \code{contentId} to target the
#'   content directly, without a local deployment record.
#' @export
showUsers <- function(
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
  account = NULL,
  server = NULL
) {
  accountDetails <- accountInfo(account, server)
  api <- clientForAccount(accountDetails)

  application <- resolveContentTarget(
    api,
    accountDetails,
    appDir,
    appName,
    contentId
  )

  listCollaborators(api, application$id)
}

#' List invited users for an application
#'
#' @description
#' List invited users for an application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application.
#' @param contentId On Posit Connect Cloud, the content ID to manage, taken from
#'   the content URL
#'   (\code{https://connect.posit.cloud/{account}/content/{contentId}}). When
#'   supplied, \code{appDir} and \code{appName} are ignored and no local
#'   deployment record is required. Not supported on shinyapps.io.
#' @inheritParams deployApp
#' @seealso [addAuthorizedUser()] and [showUsers()]
#' @note This function works for ShinyApps and Posit Connect Cloud. On Posit
#'   Connect Cloud, the \code{link} column is always \code{NA} because the
#'   accept link is only emailed to the recipient and is never returned by
#'   the API.
#'
#'   On Posit Connect Cloud, the content is resolved from the local deployment
#'   record under \code{appDir}, which defaults to the working directory. Pass
#'   \code{appDir} to point at the project directory that contains the
#'   \code{rsconnect/} deployment record. \code{appName} selects among multiple records in
#'   the same directory. Alternatively, pass \code{contentId} to target the
#'   content directly, without a local deployment record.
#' @export
showInvited <- function(
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
  account = NULL,
  server = NULL
) {
  accountDetails <- accountInfo(account, server)
  api <- clientForAccount(accountDetails)

  application <- resolveContentTarget(
    api,
    accountDetails,
    appDir,
    appName,
    contentId
  )

  listInvitations(api, application$id)
}

#' Resend invitation for invited users of an application
#'
#' @description
#' Resend invitation for invited users of an application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param invite The invitation to resend. Can be id or email address.
#' @param regenerate Regenerate the invite code. Can be helpful is the
#' invitation has expired.
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application.
#' @param contentId On Posit Connect Cloud, the content ID to manage, taken from
#'   the content URL
#'   (\code{https://connect.posit.cloud/{account}/content/{contentId}}). When
#'   supplied, \code{appDir} and \code{appName} are ignored and no local
#'   deployment record is required. Not supported on shinyapps.io.
#' @inheritParams deployApp
#' @seealso [showInvited()]
#' @note This function works for ShinyApps and Posit Connect Cloud. The
#'   invitation can be selected by id or email address. On Posit Connect Cloud,
#'   the \code{regenerate} argument has no effect.
#'
#'   On Posit Connect Cloud, the content is resolved from the local deployment
#'   record under \code{appDir}, which defaults to the working directory. Pass
#'   \code{appDir} to point at the project directory that contains the
#'   \code{rsconnect/} deployment record. \code{appName} selects among multiple records in
#'   the same directory. Alternatively, pass \code{contentId} to target the
#'   content directly, without a local deployment record.
#' @export
resendInvitation <- function(
  invite,
  regenerate = FALSE,
  appDir = getwd(),
  appName = NULL,
  contentId = NULL,
  account = NULL,
  server = NULL
) {
  accountDetails <- accountInfo(account, server)
  api <- clientForAccount(accountDetails)

  application <- resolveContentTarget(
    api,
    accountDetails,
    appDir,
    appName,
    contentId
  )
  invited <- listInvitations(api, application$id)

  invite <- as.character(invite)
  # Match id first (UUID strings on PCC, numeric-as-character on shinyapps.io),
  # then fall back to email. The old is.numeric() branch missed PCC UUID ids.
  if (invite %in% invited$id) {
    invite <- invited[which(invited$id == invite), ]
  } else if (invite %in% invited$email) {
    invite <- invited[which(invited$email == invite), ]
  } else {
    stop("Invitation for \"", invite, "\" not found", call. = FALSE)
  }

  if (is.na(invite$id)) {
    cli::cli_abort(c(
      "Cannot resend invitation for {.val {invite$email}}: the matched record has no id.",
      i = "This is unexpected; contact Posit support if this persists."
    ))
  }

  # resend invitation (api already built above)
  resendApplicationInvitation(api, invite$id, regenerate)

  message(paste("Sent invitation to", invite$email, "", sep = " "))

  invisible(TRUE)
}

# Previously exported, but deprecated since 2015
authorizedUsers <- function(appDir = getwd()) {
  # read password file
  path <- getPasswordFile(appDir)
  if (file.exists(path)) {
    passwords <- readPasswordFile(path)
  } else {
    passwords <- NULL
  }

  return(passwords)
}

validateEmail <- function(email) {
  if (is.null(email) || !grepl(".+\\@.+\\..+", email)) {
    stop("Invalid email address.", call. = FALSE)
  }

  invisible(email)
}

getPasswordFile <- function(appDir) {
  check_directory(appDir)

  file.path(normalizePath(appDir), "shinyapps", "passwords.txt")
}

readPasswordFile <- function(path) {
  # open and read file
  lines <- readLines(path)

  # extract fields
  fields <- do.call(rbind, strsplit(lines, ":"))
  users <- fields[, 1]
  hashes <- fields[, 2]

  # convert to data frame
  df <- data.frame(user = users, hash = hashes, stringsAsFactors = FALSE)

  # return data frame
  return(df)
}

writePasswordFile <- function(path, passwords) {
  # open and file
  f <- file(path, open = "w")
  defer(close(f))

  # write passwords
  apply(passwords, 1, function(r) {
    l <- paste(r[1], ":", r[2], "\n", sep = "")
    cat(l, file = f, sep = "")
  })
  message(
    "Password file updated. You must deploy your application for these changes to take effect."
  )
}
