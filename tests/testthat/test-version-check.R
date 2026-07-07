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

# latestAvailableVersion() --------------------------------------------------

test_that("latestAvailableVersion() reads the version out of available.packages()", {
  local_mocked_bindings(availablePackages = function(repos) {
    fakeAvailablePackages()
  })

  expect_equal(
    latestAvailableVersion(
      "rsconnect",
      repos = c(CRAN = "https://cran.example")
    ),
    "1.11.0"
  )
})

test_that("latestAvailableVersion() returns NULL when the package isn't listed", {
  local_mocked_bindings(
    availablePackages = function(repos) fakeAvailablePackages("some-other-pkg")
  )

  expect_null(latestAvailableVersion(
    "rsconnect",
    repos = c(CRAN = "https://cran.example")
  ))
})

test_that("latestAvailableVersion() returns NULL for the unresolved @CRAN@ placeholder", {
  # No mock: if this fell through to available.packages() it would attempt a
  # real network request and this test would hang/fail depending on network.
  expect_null(latestAvailableVersion("rsconnect", repos = c(CRAN = "@CRAN@")))
})

test_that("latestAvailableVersion() returns NULL rather than erroring", {
  local_mocked_bindings(availablePackages = function(repos) {
    stop("network is down")
  })

  expect_null(latestAvailableVersion(
    "rsconnect",
    repos = c(CRAN = "https://cran.example")
  ))
})

# checkForNewerVersion() ----------------------------------------------------

test_that("checkForNewerVersion() checks live every call (no caching)", {
  repos <- c(CRAN = "https://cran.example")

  calls <- 0
  local_mocked_bindings(latestAvailableVersion = function(...) {
    calls <<- calls + 1
    "999.0.0"
  })

  checkForNewerVersion("rsconnect", repos = repos)
  checkForNewerVersion("rsconnect", repos = repos)
  expect_equal(calls, 2)
})

test_that("checkForNewerVersion() returns NULL when not outdated", {
  local_mocked_bindings(latestAvailableVersion = function(...) "0.0.1")
  expect_null(checkForNewerVersion(
    "rsconnect",
    repos = c(CRAN = "https://cran.example")
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
    repos = c(CRAN = "https://cran.example")
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
    repos = c(CRAN = "https://cran.example")
  ))
})

# messages -------------------------------------------------------------

test_that("update messages are informative but don't overclaim", {
  expect_snapshot({
    updateStartupMessage("1.10.0", "1.11.0")
    updateErrorHint("1.10.0", "1.11.0")
  })
})
