# newerVersionAvailable() --------------------------------------------------

test_that("newerVersionAvailable() detects a newer version", {
  expect_equal(newerVersionAvailable("1.10.0", "1.11.0"), "1.11.0")
})

test_that("newerVersionAvailable() returns NULL when not newer", {
  expect_null(newerVersionAvailable("1.10.0", "1.10.0"))
  expect_null(newerVersionAvailable("1.10.0", "1.9.0"))
})

test_that("newerVersionAvailable() treats a local dev version as up to date", {
  expect_null(newerVersionAvailable("1.10.0.9000", "1.10.0"))
})

test_that("newerVersionAvailable() returns NULL when latest is unknown", {
  expect_null(newerVersionAvailable("1.10.0", NULL))
})

test_that("newerVersionAvailable() returns NULL rather than erroring on a malformed version", {
  # `latest` may come from an untrusted repo or a hand-edited/corrupted
  # cache file, so package_version() failing to parse it shouldn't throw.
  expect_null(newerVersionAvailable("1.10.0", "not-a-version!!"))
})

# parsePackagesVersion() ----------------------------------------------------

test_that("parsePackagesVersion() reads the version for a listed package", {
  packages <- "Package: rsconnect\nVersion: 1.11.0\n\nPackage: other\nVersion: 2.0.0\n"
  expect_equal(parsePackagesVersion(packages, "rsconnect"), "1.11.0")
})

test_that("parsePackagesVersion() returns NULL when the package isn't listed", {
  packages <- "Package: other\nVersion: 2.0.0\n"
  expect_null(parsePackagesVersion(packages, "rsconnect"))
})

test_that("parsePackagesVersion() returns NULL rather than erroring on garbage input", {
  expect_null(parsePackagesVersion("not a PACKAGES file at all", "rsconnect"))
})

# latestAvailableVersion() --------------------------------------------------

test_that("latestAvailableVersion() reads the version out of a repo's PACKAGES index", {
  skip_if_not_installed("webfakes")

  repo <- service_repo(c("Package: rsconnect\nVersion: 1.11.0"))
  expect_equal(
    latestAvailableVersion("rsconnect", repos = c(CRAN = repo)),
    "1.11.0"
  )
})

test_that("latestAvailableVersion() returns NULL when the package isn't listed", {
  skip_if_not_installed("webfakes")

  repo <- service_repo(c("Package: other\nVersion: 2.0.0"))
  expect_null(latestAvailableVersion("rsconnect", repos = c(CRAN = repo)))
})

test_that("latestAvailableVersion() takes the highest version across multiple repos", {
  skip_if_not_installed("webfakes")

  old_repo <- service_repo(c("Package: rsconnect\nVersion: 1.9.0"))
  new_repo <- service_repo(c("Package: rsconnect\nVersion: 1.11.0"))

  expect_equal(
    latestAvailableVersion(
      "rsconnect",
      repos = c(old = old_repo, new = new_repo)
    ),
    "1.11.0"
  )
})

test_that("latestAvailableVersion() returns NULL for the unresolved @CRAN@ placeholder", {
  expect_null(latestAvailableVersion("rsconnect", repos = c(CRAN = "@CRAN@")))
})

test_that("latestAvailableVersion() returns NULL rather than erroring when a repo 404s", {
  skip_if_not_installed("webfakes")

  # service_repo() only serves /src/contrib/PACKAGES, so any other path 404s.
  app <- webfakes::new_app_process(webfakes::new_app())
  expect_null(latestAvailableVersion("rsconnect", repos = c(CRAN = app$url())))
})

test_that("latestAvailableVersion() returns NULL rather than erroring on a malformed version", {
  skip_if_not_installed("webfakes")

  # A broken or hand-rolled internal mirror -- exactly the kind of repo
  # this feature needs to tolerate -- could list a Version field
  # package_version() can't parse.
  repo <- service_repo(c("Package: rsconnect\nVersion: not-a-version!!"))
  expect_null(latestAvailableVersion("rsconnect", repos = c(CRAN = repo)))
})

test_that("latestAvailableVersion() ignores a malformed version from one repo among several", {
  skip_if_not_installed("webfakes")

  broken <- service_repo(c("Package: rsconnect\nVersion: not-a-version!!"))
  ok <- service_repo(c("Package: rsconnect\nVersion: 5.0.0"))

  expect_equal(
    latestAvailableVersion("rsconnect", repos = c(broken = broken, ok = ok)),
    "5.0.0"
  )
})

test_that("latestAvailableVersion() is bounded by rsconnect.update_check_timeout", {
  skip_if_not_installed("webfakes")
  skip_on_cran() # timing-sensitive

  withr::local_options(rsconnect.update_check_timeout = 1)
  # Repo takes 10s to respond -- if the timeout isn't honored, this test
  # itself would take 10+ seconds instead of ~1.
  repo <- service_repo(c("Package: rsconnect\nVersion: 1.11.0"), delay = 10)

  elapsed <- system.time(
    result <- latestAvailableVersion("rsconnect", repos = c(CRAN = repo))
  )
  expect_null(result)
  expect_lt(elapsed[["elapsed"]], 5)
})

# reposCacheKey() / disk cache round trip ------------------------------------

test_that("reposCacheKey() survives a write.dcf()/read.dcf() round trip", {
  repos <- c(
    PPM = "https://packagemanager.posit.co/cran/latest",
    RSPM = "https://packagemanager.posit.co/cran/latest",
    CRAN = "https://packagemanager.posit.co/cran/latest"
  )

  cacheDir <- withr::local_tempdir()
  writeVersionCheckCache("rsconnect", "1.11.0", reposCacheKey(repos), cacheDir)
  cached <- readVersionCheckCache("rsconnect", cacheDir)

  expect_identical(cached$repos, reposCacheKey(repos))
})

test_that("reposCacheKey() differs for different repos", {
  a <- reposCacheKey(c(CRAN = "https://cran.example"))
  b <- reposCacheKey(c(CRAN = "https://other.example"))
  expect_false(identical(a, b))
})

# cachedLatestVersion() ------------------------------------------------------

test_that("cachedLatestVersion() caches on disk and throttles refreshes", {
  cacheDir <- withr::local_tempdir()
  repos <- c(CRAN = "https://cran.example")

  calls <- 0
  local_mocked_bindings(latestAvailableVersion = function(...) {
    calls <<- calls + 1
    "1.11.0"
  })

  expect_equal(cachedLatestVersion("rsconnect", repos, cacheDir), "1.11.0")
  expect_equal(calls, 1)

  # Clear the session-level cache so the second call is forced through the
  # disk-read path -- otherwise this test would pass even if the on-disk
  # cache were broken (e.g. never actually recognized as fresh on read).
  rm(list = ls(versionCheckCache), envir = versionCheckCache)

  # Within the throttle interval, a second call should read the disk cache
  # instead of checking again.
  expect_equal(cachedLatestVersion("rsconnect", repos, cacheDir), "1.11.0")
  expect_equal(calls, 1)
})

test_that("cachedLatestVersion() caches a failed/unknown lookup too", {
  cacheDir <- withr::local_tempdir()
  repos <- c(CRAN = "https://cran.example")

  calls <- 0
  local_mocked_bindings(latestAvailableVersion = function(...) {
    calls <<- calls + 1
    NULL
  })

  expect_null(cachedLatestVersion("rsconnect", repos, cacheDir))
  expect_null(cachedLatestVersion("rsconnect", repos, cacheDir))
  # An unreachable/unknown result should still only be checked once per
  # interval, not retried on every call.
  expect_equal(calls, 1)
})

test_that("cachedLatestVersion() refreshes a stale cache", {
  cacheDir <- withr::local_tempdir()
  repos <- c(CRAN = "https://cran.example")

  dcf <- cbind(
    Checked = as.character(as.numeric(Sys.time()) - 999999),
    Latest = "1.9.0",
    Repos = reposCacheKey(repos)
  )
  write.dcf(dcf, versionCheckCacheFile("rsconnect", cacheDir))

  local_mocked_bindings(latestAvailableVersion = function(...) "1.11.0")

  expect_equal(cachedLatestVersion("rsconnect", repos, cacheDir), "1.11.0")
})

test_that("cachedLatestVersion() refreshes when repos change", {
  cacheDir <- withr::local_tempdir()
  oldRepos <- c(CRAN = "https://old.example")
  writeVersionCheckCache(
    "rsconnect",
    "1.9.0",
    reposCacheKey(oldRepos),
    cacheDir
  )

  local_mocked_bindings(latestAvailableVersion = function(...) "1.11.0")

  expect_equal(
    cachedLatestVersion("rsconnect", c(CRAN = "https://new.example"), cacheDir),
    "1.11.0"
  )
})

# checkForNewerVersion() ----------------------------------------------------

test_that("checkForNewerVersion() returns NULL when not outdated", {
  withr::local_options(rsconnect.check_updates = TRUE)
  cacheDir <- withr::local_tempdir()
  local_mocked_bindings(latestAvailableVersion = function(...) "0.0.1")

  expect_null(checkForNewerVersion(
    "rsconnect",
    repos = c(CRAN = "https://cran.example"),
    cacheDir = cacheDir
  ))
})

test_that("checkForNewerVersion() can be disabled via option", {
  withr::local_options(rsconnect.check_updates = FALSE)
  local_mocked_bindings(
    latestAvailableVersion = function(...) {
      testthat::fail("should not be called")
    }
  )

  expect_null(checkForNewerVersion(
    "rsconnect",
    repos = c(CRAN = "https://cran.example"),
    cacheDir = withr::local_tempdir()
  ))
})

test_that("checkForNewerVersion() can be disabled via environment variable", {
  withr::local_envvar(RSCONNECT_CHECK_UPDATES = "false")
  local_mocked_bindings(
    latestAvailableVersion = function(...) {
      testthat::fail("should not be called")
    }
  )

  expect_null(checkForNewerVersion(
    "rsconnect",
    repos = c(CRAN = "https://cran.example"),
    cacheDir = withr::local_tempdir()
  ))
})

test_that("checkForNewerVersion() env var disable is not case-sensitive", {
  # RSCONNECT_PACKRAT and friends accept "TRUE"/"True"/"1" etc. via truthy();
  # RSCONNECT_CHECK_UPDATES should behave the same way, not just react to the
  # exact lowercase string "false".
  withr::local_envvar(RSCONNECT_CHECK_UPDATES = "FALSE")
  local_mocked_bindings(
    latestAvailableVersion = function(...) {
      testthat::fail("should not be called")
    }
  )

  expect_null(checkForNewerVersion(
    "rsconnect",
    repos = c(CRAN = "https://cran.example"),
    cacheDir = withr::local_tempdir()
  ))
})

test_that("checkForNewerVersion() environment variable takes precedence over the option", {
  withr::local_options(rsconnect.check_updates = FALSE)
  withr::local_envvar(RSCONNECT_CHECK_UPDATES = "true")
  local_mocked_bindings(latestAvailableVersion = function(...) "999.0.0")

  expect_equal(
    checkForNewerVersion(
      "rsconnect",
      repos = c(CRAN = "https://cran.example"),
      cacheDir = withr::local_tempdir()
    ),
    "999.0.0"
  )
})

# messages -------------------------------------------------------------

test_that("update messages are informative but don't overclaim", {
  expect_snapshot({
    updateStartupMessage("1.10.0", "1.11.0")
    updateErrorHint("1.10.0", "1.11.0")
  })
})

test_that("deploy version message mentions latest version only when known", {
  expect_snapshot({
    deployVersionMessage("1.10.0")
    deployVersionMessage("1.10.0", "1.11.0")
  })
})
