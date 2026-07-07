# rsconnect checks whether a newer version of itself is available from the
# user's configured repositories, and surfaces that information in two places:
#
#   * a startup message when the package is attached interactively, since
#     that's the moment a user is most likely to notice it (see `.onAttach()`
#     in `zzz.R`)
#   * a hint appended to HTTP errors, since push-button deployment from the
#     IDE loads rsconnect without attaching it, so the startup message never
#     fires (see `reportError()` in `http.R`)
#
# The check never blocks deployment or throws: any failure (offline,
# air-gapped repositories, timeout, etc.) is swallowed and simply results in
# no notifiication.

# Returns the newest version of `pkg` available from `repos`, or NULL if it
# can't be determined.
latestAvailableVersion <- function(
  pkg = "rsconnect",
  repos = getOption("repos")
) {
  repos <- repos[repos != "@CRAN@"]
  if (length(repos) == 0) {
    return(NULL)
  }

  old_timeout <- options(
    timeout = getOption("rsconnect.update_check_timeout", 5)
  )
  on.exit(options(old_timeout), add = TRUE)

  tryCatch(
    suppressWarnings({
      db <- availablePackages(repos)
      idx <- match(pkg, db[, "Package"])
      if (is.na(idx)) {
        NULL
      } else {
        unname(db[idx, "Version"])
      }
    }),
    error = function(e) NULL
  )
}

# Returns `latest` if it represents a version newer than `installed`,
# otherwise NULL. A locally-built dev version (e.g. 1.10.0.9000) is never
# considered outdated.
newerVersionAvailable <- function(installed, latest) {
  if (is.null(latest)) {
    return(NULL)
  }

  if (package_version(latest) > package_version(installed)) {
    latest
  } else {
    NULL
  }
}

checkUpdatesEnabled <- function() {
  isTRUE(getOption("rsconnect.check_updates", TRUE)) &&
    !identical(tolower(Sys.getenv("RSCONNECT_CHECK_UPDATES", "true")), "false")
}

# The single entry point: returns the newer version of `pkg` available from
# `repos`, if any, or NULL. Always checks live (no caching), respecting
# `rsconnect.check_updates`/`RSCONNECT_CHECK_UPDATES` as an opt-out.
checkForNewerVersion <- function(
  pkg = "rsconnect",
  repos = getOption("repos")
) {
  if (!checkUpdatesEnabled()) {
    return(NULL)
  }

  installed <- utils::packageVersion(pkg)
  newerVersionAvailable(installed, latestAvailableVersion(pkg, repos))
}

# Shown at package attach time when a newer version is available.
updateStartupMessage <- function(installed, latest) {
  paste0(
    "A newer version of rsconnect (",
    latest,
    ") is available from your repositories. You have ",
    installed,
    "."
  )
}

# A neutral bullet appended to HTTP error messages. Deliberately avoids
# implying that updating will resolve the error -- rsconnect has no way of
# knowing whether an outdated version caused the failure.
updateErrorHint <- function(installed, latest) {
  c(
    i = sprintf(
      "rsconnect %s is available from your repositories (you have %s).",
      latest,
      installed
    )
  )
}
