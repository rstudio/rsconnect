# Helper to create a clean API key cache for tests
local_api_key_cache <- function(env = caller_env()) {
  nms <- env_names(apiKeyCache)
  zaps <- rep_named(nms, list(zap()))

  old <- env_bind(apiKeyCache, !!!zaps)
  withr::defer(env_bind(apiKeyCache, !!!old), envir = env)
}

test_that("cache stores and retrieves API keys with expiry", {
  local_api_key_cache()

  expect_null(getCachedApiKey("https://example.com"))

  # Cache with future expiry
  future_expiry <- Sys.time() + 3600
  cacheApiKey("https://example.com", "test-api-key", future_expiry)
  expect_equal(getCachedApiKey("https://example.com"), "test-api-key")
})

test_that("cache returns NULL for expired keys", {
  local_api_key_cache()

  # Cache with past expiry (already expired)
  past_expiry <- Sys.time() - 100
  cacheApiKey("https://example.com", "expired-key", past_expiry)

  # Should return NULL because key is expired
  expect_null(getCachedApiKey("https://example.com"))
})

test_that("cache respects expiry buffer", {
  local_api_key_cache()

  # Cache with expiry just inside the buffer (should be treated as expired)
  # Default buffer is 60 seconds
  almost_expired <- Sys.time() + 30
  cacheApiKey("https://example.com", "almost-expired-key", almost_expired)

  # Should return NULL because within buffer
  expect_null(getCachedApiKey("https://example.com"))
})

test_that("cache works without expiry (NULL expiry)", {
  local_api_key_cache()

  # Cache without expiry
  cacheApiKey("https://example.com", "no-expiry-key", NULL)

  # Should still return the key
  expect_equal(getCachedApiKey("https://example.com"), "no-expiry-key")
})

test_that("attemptIdentityFederation returns cached key if available and not expired", {
  local_api_key_cache()

  cacheApiKey("https://example.com", "cached-api-key", Sys.time() + 3600)

  # Even without Workbench env var, should return cached key
  withr::local_envvar(POSIT_PRODUCT = "", RS_SERVER_ADDRESS = "")

  result <- attemptIdentityFederation("https://example.com")
  expect_equal(result, "cached-api-key")
})

test_that("attemptIdentityFederation returns NULL when not in Workbench", {
  local_api_key_cache()

  withr::local_envvar(POSIT_PRODUCT = "", RS_SERVER_ADDRESS = "")

  expect_null(attemptIdentityFederation("https://example.com"))
})

test_that("hasNoCredentials correctly detects missing credentials", {
  # No credentials at all
  expect_true(hasNoCredentials(list(server = "example.com")))

  # Has apiKey
  expect_false(hasNoCredentials(list(server = "example.com", apiKey = "key")))

  # Has token
  expect_false(hasNoCredentials(list(server = "example.com", token = "tok")))

  # Has secret
  expect_false(hasNoCredentials(list(server = "example.com", secret = "sec")))

  # Has private_key
  expect_false(
    hasNoCredentials(list(server = "example.com", private_key = "pk"))
  )

  # Has accessToken
  expect_false(
    hasNoCredentials(list(server = "example.com", accessToken = "at"))
  )
})

test_that("clientForAccount attempts identity federation for Connect without credentials", {
  local_temp_config()
  local_api_key_cache()

  addTestServer("example.com")

  # Mock successful identity federation
  local_mocked_bindings(
    attemptIdentityFederation = function(serverUrl) "ephemeral-api-key"
  )

  account <- list(server = "example.com")
  client <- clientForAccount(account)

  expect_equal(client$service(), "connect")
})

test_that("clientForAccount skips identity federation when credentials exist", {
  local_temp_config()
  local_api_key_cache()

  addTestServer("example.com")

  # Mock - should not be called
  attempted <- FALSE
  local_mocked_bindings(
    attemptIdentityFederation = function(serverUrl) {
      attempted <<- TRUE
      "ephemeral-api-key"
    }
  )

  # Account with existing API key
  account <- list(server = "example.com", apiKey = "existing-key")
  client <- clientForAccount(account)

  expect_equal(client$service(), "connect")
  expect_false(attempted)
})

test_that("clientForAccount skips identity federation for ShinyApps", {
  # Mock - should not be called
  attempted <- FALSE
  local_mocked_bindings(
    attemptIdentityFederation = function(serverUrl) {
      attempted <<- TRUE
      "ephemeral-api-key"
    }
  )

  account <- list(server = "shinyapps.io")
  client <- clientForAccount(account)

  expect_equal(client$service(), "shinyapps.io")
  expect_false(attempted)
})

test_that("clientForAccount skips identity federation for Connect Cloud", {
  # Mock - should not be called
  attempted <- FALSE
  local_mocked_bindings(
    attemptIdentityFederation = function(serverUrl) {
      attempted <<- TRUE
      "ephemeral-api-key"
    }
  )

  account <- list(server = "connect.posit.cloud")
  client <- clientForAccount(account)

  expect_equal(client$service(), "connect.posit.cloud")
  expect_false(attempted)
})
