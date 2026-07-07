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
# no notification. The result (including a failed/unknown result) is cached
# to disk for rsconnect.update_check_interval seconds (default 1 day), so
# the network is only actually touched once per interval.

# Fetches and parses a single repo's PACKAGES index, returning the version
# listed for `pkg`, or NULL if it can't be determined. Uses httr2 rather
# than available.packages()/download.file() because httr2::req_timeout()
# reliably bounds the connection phase too -- options(timeout=) does not.
latestVersionInRepo <- function(pkg, repo, timeout) {
  url <- paste0(sub("/+$", "", repo), "/src/contrib/PACKAGES")

  packages <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform() |>
      httr2::resp_body_string(),
    error = function(e) NULL
  )

  if (is.null(packages)) {
    return(NULL)
  }

  parsePackagesVersion(packages, pkg)
}

# Pure parsing step, split out from latestVersionInRepo() so it's testable
# without a network call.
parsePackagesVersion <- function(packagesText, pkg) {
  tryCatch(
    {
      fields <- read.dcf(textConnection(packagesText))
      idx <- match(pkg, fields[, "Package"])
      if (is.na(idx)) NULL else unname(fields[idx, "Version"])
    },
    error = function(e) NULL
  )
}

# Returns the newest version of `pkg` available from `repos`, or NULL if it
# can't be determined.
latestAvailableVersion <- function(pkg = "rsconnect", repos = getOption("repos")) {
  repos <- repos[repos != "@CRAN@"]
  if (length(repos) == 0) {
    return(NULL)
  }

  timeout <- getOption("rsconnect.update_check_timeout", 2)
  versions <- unlist(lapply(repos, latestVersionInRepo, pkg = pkg, timeout = timeout))
  versions <- versions[vapply(versions, isValidVersion, logical(1))]
  if (length(versions) == 0) {
    NULL
  } else {
    as.character(max(package_version(versions)))
  }
}

# Whether `x` is a string package_version() can parse, without throwing.
isValidVersion <- function(x) {
  !is.null(tryCatch(package_version(x), error = function(e) NULL))
}

# Returns `latest` if it represents a version newer than `installed`,
# otherwise NULL. `latest` may come from an untrusted source (a repo's
# PACKAGES file, or a cache file written by a hand-edited or future/past
# version of this code) so a value package_version() can't parse is treated
# as "not newer" rather than thrown.
newerVersionAvailable <- function(installed, latest) {
  if (is.null(latest)) {
    return(NULL)
  }

  newer <- tryCatch(
    package_version(latest) > package_version(installed),
    error = function(e) FALSE
  )

  if (newer) latest else NULL
}

checkUpdatesEnabled <- function() {
  # Use RSCONNECT_CHECK_UPDATES when it has any value; fall back to
  # rsconnect.check_updates when the environment variable is unset.
  value <- Sys.getenv("RSCONNECT_CHECK_UPDATES", unset = NA)
  if (is.na(value)) {
    value <- getOption("rsconnect.check_updates", default = TRUE)
  }

  truthy(value, default = TRUE)
}

# Cache ---------------------------------------------------------------------
#
# A session-level environment cache backed by an on-disk .dcf file under
# rsconnectConfigDir("cache"). Entries -- including a failed/unknown lookup
# -- are fresh for rsconnect.update_check_interval seconds, and invalidated
# if the configured repos change.

versionCheckCache <- new.env(parent = emptyenv())

versionCheckCacheFile <- function(pkg, cacheDir) {
  file.path(cacheDir, paste0(pkg, "-update-check.dcf"))
}

readVersionCheckCache <- function(pkg, cacheDir) {
  path <- versionCheckCacheFile(pkg, cacheDir)
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    {
      dcf <- read.dcf(path)
      list(
        checked = as.numeric(dcf[1, "Checked"]),
        latest = if (nzchar(dcf[1, "Latest"])) unname(dcf[1, "Latest"]) else NULL,
        repos = unname(dcf[1, "Repos"])
      )
    },
    error = function(e) NULL
  )
}

writeVersionCheckCache <- function(pkg, latest, reposKey, cacheDir) {
  dcf <- cbind(
    Checked = as.character(as.numeric(Sys.time())),
    Latest = if (is.null(latest)) "" else latest,
    Repos = reposKey
  )
  tryCatch(
    write.dcf(dcf, versionCheckCacheFile(pkg, cacheDir)),
    error = function(e) NULL
  )
  invisible()
}

# A short, fixed-length key identifying a set of repos, used to invalidate
# the cache if the user's configured repos change. Deliberately not the raw
# joined URLs: DCF folds long field values across multiple lines, and
# read.dcf() doesn't reliably restore the original whitespace, which would
# make a multi-repo (or just long-URL) key never compare equal to itself
# after a round trip through the cache file.
reposCacheKey <- function(repos) {
  digest::digest(repos, algo = "xxhash32")
}

# Returns the latest known version of `pkg` from `repos`, refreshing from
# the network only if the cache is missing, older than
# rsconnect.update_check_interval seconds (default 1 day), or was recorded
# for a different set of repos. A failed/unknown lookup is cached too, so an
# unreachable repo is retried at most once per interval, not on every call.
cachedLatestVersion <- function(pkg, repos, cacheDir) {
  reposKey <- reposCacheKey(repos)
  sessionKey <- paste(pkg, cacheDir, sep = "|")

  cached <- env_get(versionCheckCache, sessionKey, default = NULL)
  if (is.null(cached)) {
    cached <- readVersionCheckCache(pkg, cacheDir)
  }

  interval <- getOption("rsconnect.update_check_interval", 60 * 60 * 24)
  fresh <- !is.null(cached) &&
    identical(cached$repos, reposKey) &&
    (as.numeric(Sys.time()) - cached$checked) < interval

  if (fresh) {
    latest <- cached$latest
  } else {
    latest <- latestAvailableVersion(pkg, repos)
    writeVersionCheckCache(pkg, latest, reposKey, cacheDir)
  }

  env_poke(
    versionCheckCache,
    sessionKey,
    list(checked = as.numeric(Sys.time()), latest = latest, repos = reposKey)
  )

  latest
}

# The single entry point: returns the newer version of `pkg` available from
# `repos`, if any, or NULL, using the cache above. Respects
# `rsconnect.check_updates`/`RSCONNECT_CHECK_UPDATES` as an opt-out.
checkForNewerVersion <- function(
  pkg = "rsconnect",
  repos = getOption("repos"),
  cacheDir = rsconnectConfigDir("cache")
) {
  if (!checkUpdatesEnabled()) {
    return(NULL)
  }

  installed <- utils::packageVersion(pkg)
  newerVersionAvailable(installed, cachedLatestVersion(pkg, repos, cacheDir))
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
