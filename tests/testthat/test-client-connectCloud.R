test_that("awaitCompletion", {
  skip_if_not_installed("webfakes")

  revision_app <- webfakes::new_app()
  revision_app$use(webfakes::mw_json())
  revision_app$get("/revisions/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        publish_result = "success",
        url = "https://example.posit.cloud/content/123",
        publish_error_details = NULL
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::new_app_process(revision_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    secret = NULL,
    private_key = NULL,
    apiKey = "the-api-key",
    protocol = "https",
    certificate = NULL
  )
  client <- connectCloudClient(service, authInfo)

  # test successful completion
  result <- client$awaitCompletion("rev123")
  expect_true(result$success)
  expect_equal(result$url, "https://example.posit.cloud/content/123")
  expect_null(result$error)
})

test_that("awaitCompletion handles failure", {
  skip_if_not_installed("webfakes")

  revision_app <- webfakes::new_app()
  revision_app$use(webfakes::mw_json())
  revision_app$get("/revisions/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        publish_result = "failure",
        url = NULL,
        publish_error_details = "Deployment failed due to missing dependencies"
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::new_app_process(revision_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    secret = NULL,
    private_key = NULL,
    apiKey = "the-api-key",
    protocol = "https",
    certificate = NULL
  )
  client <- connectCloudClient(service, authInfo)

  # test failure case
  result <- client$awaitCompletion("rev456")
  expect_false(result$success)
  expect_null(result$url)
  expect_equal(result$error, "Deployment failed due to missing dependencies")
})

test_that("withTokenRefreshRetry passes through successful requests", {
  skip_if_not_installed("webfakes")

  # Mock a successful API call
  mock_request_fn <- function(service, authInfo, path) {
    list(success = TRUE, data = "test response")
  }

  service <- list(host = "example.com", port = 443, protocol = "https")
  authInfo <- list(
    server = "connect.posit.cloud",
    name = "test-user",
    accountId = "123",
    access_token = "current-token",
    refresh_token = "refresh-token"
  )

  result <- withTokenRefreshRetry(service, authInfo, mock_request_fn, "/test")

  expect_equal(result$success, TRUE)
  expect_equal(result$data, "test response")
})

test_that("withTokenRefreshRetry handles 401 with successful token refresh", {
  skip_if_not_installed("webfakes")

  call_count <- 0
  mock_request_fn <- function(service, authInfo, path) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      # First call fails with 401
      err <- structure(
        list(message = "HTTP 401"),
        class = c("rsconnect_http_401", "rsconnect_http", "error", "condition")
      )
      stop(err)
    } else {
      # Second call succeeds
      list(success = TRUE, data = "success after refresh")
    }
  }

  # Mock cloudAuthClient
  mockery::stub(withTokenRefreshRetry, "cloudAuthClient", function() {
    list(
      exchangeToken = function(request) {
        expect_equal(request$grant_type, "refresh_token")
        expect_equal(request$refresh_token, "refresh-token")
        list(
          access_token = "new-access-token",
          refresh_token = "new-refresh-token"
        )
      }
    )
  })

  # Mock registerAccount
  register_called <- FALSE
  mockery::stub(
    withTokenRefreshRetry,
    "registerAccount",
    function(server, name, accountId, accessToken, refreshToken) {
      register_called <<- TRUE
      expect_equal(server, "connect.posit.cloud")
      expect_equal(name, "test-user")
      expect_equal(accountId, "123")
      expect_equal(accessToken, "new-access-token")
      expect_equal(refreshToken, "new-refresh-token")
    }
  )

  service <- list(host = "example.com", port = 443, protocol = "https")
  authInfo <- list(
    server = "connect.posit.cloud",
    name = "test-user",
    accountId = "123",
    access_token = "current-token",
    refresh_token = "refresh-token"
  )

  result <- withTokenRefreshRetry(service, authInfo, mock_request_fn, "/test")

  expect_equal(result$success, TRUE)
  expect_equal(result$data, "success after refresh")
  expect_equal(call_count, 2)
  expect_true(register_called)
})

test_that("withTokenRefreshRetry re-throws 401 when token refresh fails", {
  skip_if_not_installed("webfakes")

  mock_request_fn <- function(service, authInfo, path) {
    # Always fails with 401
    err <- structure(
      list(message = "HTTP 401"),
      class = c("rsconnect_http_401", "rsconnect_http", "error", "condition")
    )
    stop(err)
  }

  # Mock cloudAuthClient to fail
  mockery::stub(withTokenRefreshRetry, "cloudAuthClient", function() {
    list(
      exchangeToken = function(request) {
        stop("Token refresh failed")
      }
    )
  })

  service <- list(host = "example.com", port = 443, protocol = "https")
  authInfo <- list(
    server = "connect.posit.cloud",
    name = "test-user",
    accountId = "123",
    access_token = "current-token",
    refresh_token = "refresh-token"
  )

  expect_error(
    withTokenRefreshRetry(service, authInfo, mock_request_fn, "/test"),
    class = "rsconnect_http_401"
  )
})

test_that("withTokenRefreshRetry re-throws 401 when no refresh token available", {
  skip_if_not_installed("webfakes")

  mock_request_fn <- function(service, authInfo, path) {
    # Always fails with 401
    err <- structure(
      list(message = "HTTP 401"),
      class = c("rsconnect_http_401", "rsconnect_http", "error", "condition")
    )
    stop(err)
  }

  service <- list(host = "example.com", port = 443, protocol = "https")
  authInfo <- list(
    server = "connect.posit.cloud",
    name = "test-user",
    accountId = "123",
    access_token = "current-token"
    # No refresh_token
  )

  expect_error(
    withTokenRefreshRetry(service, authInfo, mock_request_fn, "/test"),
    class = "rsconnect_http_401"
  )
})
