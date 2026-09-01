# Internal: fetch authorized users for an already-resolved application id.
# Does NOT call resolveContentTarget(); callers are responsible for resolving
# content exactly once before invoking this.
# isPCC = TRUE adds display_name and role columns from the PCC response shape
# ({user: {id, email, display_name, ...}, role}); shinyapps.io keeps the
# original three-column shape (id, email, account).
showUsers_impl <- function(api, applicationId, isPCC = FALSE) {
  res <- api$listApplicationAuthorization(applicationId)
  rows <- lapply(res, function(x) {
    id <- as.character(x$user$id %||% NA_character_)
    email <- as.character(x$user$email %||% NA_character_)
    if (is.na(id) && is.na(email)) {
      backend <- if (isPCC) "Posit Connect Cloud" else "shinyapps.io"
      cli::cli_abort(
        c(
          "Unexpected response from {backend}: a user record has neither an {.field id} nor an {.field email}.",
          i = "The response shape may have changed; contact Posit support if this persists."
        )
      )
    }
    if (isPCC) {
      data.frame(
        id = id,
        email = email,
        account = NA_character_,
        display_name = as.character(x$user$display_name %||% NA_character_),
        role = as.character(x$role %||% NA_character_),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        id = id,
        email = email,
        account = if (!is.null(x$account)) {
          as.character(x$account)
        } else {
          NA_character_
        },
        stringsAsFactors = FALSE
      )
    }
  })
  if (length(rows) == 0L) {
    if (isPCC) {
      return(data.frame(
        id = character(),
        email = character(),
        account = character(),
        display_name = character(),
        role = character(),
        stringsAsFactors = FALSE
      ))
    }
    return(data.frame(
      id = character(),
      email = character(),
      account = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# Internal: fetch pending invitations for an already-resolved application id.
# Does NOT call resolveContentTarget().
showInvited_impl <- function(api, applicationId) {
  res <- api$listApplicationInvitations(applicationId)
  # PCC uses email_address / is_expired; shinyapps.io uses email / expired.
  rows <- lapply(res, function(x) {
    data.frame(
      id = as.character(x$id %||% NA_character_),
      email = as.character(x$email_address %||% x$email %||% NA_character_),
      link = as.character(x$link %||% NA_character_),
      expired = as.logical(x$is_expired %||% x$expired %||% NA),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    return(data.frame(
      id = character(),
      email = character(),
      link = character(),
      expired = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
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

# Internal: resolve the target content for collaborator management functions.
# On PCC, an explicit contentId targets the content directly; otherwise reads
# the local deployment record to get the content id (appId) rather than matching
# by title (mutable, non-unique on PCC).
# On shinyapps.io, delegates to resolveApplication() unchanged; contentId is
# not supported there.
resolveContentTarget <- function(
  accountDetails,
  appDir,
  appName,
  contentId = NULL
) {
  if (isPositConnectCloudServer(accountDetails$server)) {
    # An explicit content id targets PCC content directly, with no local
    # deployment record required.
    if (!is.null(contentId)) {
      check_string(contentId)
      return(list(id = contentId))
    }
    recs <- deployments(
      appPath = appDir,
      accountFilter = accountDetails$name,
      serverFilter = accountDetails$server,
      nameFilter = appName
    )
    if (nrow(recs) == 0L) {
      cli::cli_abort(c(
        "Can't identify the Posit Connect Cloud content for {.file {appDir}}.",
        i = paste0(
          "No deployment record found. Deploy the content first, or run from ",
          "the project directory that contains its {.path rsconnect/} deployment record."
        )
      ))
    }
    if (nrow(recs) > 1L) {
      dep <- disambiguateDeployments(recs)
      return(list(id = dep$appId))
    }
    list(id = recs$appId[[1L]])
  } else {
    if (!is.null(contentId)) {
      cli::cli_abort(c(
        "{.arg contentId} is only supported on Posit Connect Cloud.",
        i = "On shinyapps.io, identify the application with {.arg appName}."
      ))
    }
    resolveApplication(accountDetails, appName %||% basename(appDir))
  }
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
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  application <- resolveContentTarget(
    accountDetails,
    appDir,
    appName,
    contentId
  )

  # check for and remove password file (shinyapps.io only; PCC has no password file)
  if (!isPositConnectCloudServer(accountDetails$server)) {
    cleanupPasswordFile(appDir)
  }

  # PCC always emails invitees; warn only when caller explicitly opts out
  if (
    isPositConnectCloudServer(accountDetails$server) &&
      identical(sendEmail, FALSE)
  ) {
    cli::cli_warn(
      "{.arg sendEmail} is ignored on Posit Connect Cloud; PCC always sends an invitation email."
    )
  }

  # fetch authorization list
  api <- clientForAccount(accountDetails)
  api$inviteApplicationUser(
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
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  application <- resolveContentTarget(
    accountDetails,
    appDir,
    appName,
    contentId
  )

  # check and remove password file (shinyapps.io only; PCC has no password file)
  if (!isPositConnectCloudServer(accountDetails$server)) {
    cleanupPasswordFile(appDir)
  }

  # resolve content exactly once: use impl so showUsers() does not call
  # resolveContentTarget() a second time (a second interactive prompt could
  # return a different record, causing removeApplicationUser to act on the
  # wrong content).
  api <- clientForAccount(accountDetails)
  users <- showUsers_impl(
    api,
    application$id,
    isPCC = isPositConnectCloudServer(accountDetails$server)
  )

  user <- as.character(user)
  # Match id first (UUID strings on PCC, numeric-as-character on shinyapps.io),
  # then fall back to email. The old is.numeric() branch missed PCC UUID ids.
  if (user %in% users$id) {
    user <- users[which(users$id == user), ]
  } else if (user %in% users$email) {
    user <- users[which(users$email == user), ]
  } else {
    # Only PCC redacts emails, and the hint only helps someone who searched by
    # email (an id-based lookup already avoids the problem).
    redactionHint <-
      isPositConnectCloudServer(accountDetails$server) &&
      grepl("@", user, fixed = TRUE)
    cli::cli_abort(c(
      "User {.val {user}} not found.",
      i = if (redactionHint) {
        "On Posit Connect Cloud an email can be redacted and won't match; pass the user id from {.fn showUsers} instead."
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
  api$removeApplicationUser(application$id, user$id)

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
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  application <- resolveContentTarget(
    accountDetails,
    appDir,
    appName,
    contentId
  )

  api <- clientForAccount(accountDetails)
  showUsers_impl(
    api,
    application$id,
    isPCC = isPositConnectCloudServer(accountDetails$server)
  )
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
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  application <- resolveContentTarget(
    accountDetails,
    appDir,
    appName,
    contentId
  )

  api <- clientForAccount(accountDetails)
  showInvited_impl(api, application$id)
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
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  # resolve content exactly once, then fetch invitations via impl (avoids a
  # second resolveContentTarget() call).
  application <- resolveContentTarget(
    accountDetails,
    appDir,
    appName,
    contentId
  )
  api <- clientForAccount(accountDetails)
  invited <- showInvited_impl(api, application$id)

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
  api$resendApplicationInvitation(invite$id, regenerate)

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
