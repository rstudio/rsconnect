test_that("awaitCompletion", {
  skip_if_not_installed("webfakes")

  revision_app <- webfakes::new_app()
  revision_app$use(webfakes::mw_json())
  revision_app$get("/revisions/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        content_id = "content789",
        publish_result = "success",
        status = "published",
        url = "https://example.posit.cloud/content/123",
        publish_error_details = NULL
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::new_app_process(revision_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    username = "some-user",
    accountId = "123",
    accessToken = "current-token",
    refreshToken = "refresh-token"
  )
  client <- connectCloudClient(service, authInfo)

  # test successful completion
  result <- client$awaitCompletion("rev123")
  expect_true(result$success)
  expect_equal(
    result$url,
    "https://connect.posit.cloud/some-user/content/content789"
  )
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
        content_id = "content789",
        publish_result = "failure",
        status = "published",
        url = NULL,
        publish_error_details = "Deployment failed due to missing dependencies"
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::new_app_process(revision_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    username = "some-user",
    accountId = "123",
    accessToken = "current-token",
    refreshToken = "refresh-token"
  )
  client <- connectCloudClient(service, authInfo)

  # test failure case
  result <- client$awaitCompletion("rev456")
  expect_false(result$success)
  expect_equal(
    result$url,
    "https://connect.posit.cloud/some-user/content/content789"
  )
  expect_equal(result$error, "Deployment failed due to missing dependencies")
})

test_that("awaitCompletion handles failure with logs", {
  skip_if_not_installed("webfakes")

  # Mock revision API that returns failure with log channel
  cloudApiApp <- webfakes::new_app()
  cloudApiApp$use(webfakes::mw_json())
  cloudApiApp$get("/revisions/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        content_id = "content789",
        publish_result = "failure",
        status = "published",
        url = NULL,
        publish_error_details = "Deployment failed due to missing dependencies",
        publish_log_channel = "log-channel-123"
      ),
      auto_unbox = TRUE
    )
  })

  # Mock authorization API
  cloudApiApp$post("/authorization", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        authorized = TRUE,
        token = "auth-token-xyz"
      ),
      auto_unbox = TRUE
    )
  })

  # Mock logs API
  logs_app <- webfakes::new_app()
  logs_app$use(webfakes::mw_json())
  logs_app$get("/v1/logs/:channel", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        data = list(
          list(
            timestamp = 1234567890 * 1000000,
            message = "Starting deployment...",
            level = "info"
          ),
          list(
            timestamp = 1234567891 * 1000000,
            message = "Your app is busted!!",
            level = "error"
          )
        )
      ),
      auto_unbox = TRUE
    )
  })

  # Start the main app and logs app
  app <- webfakes::new_app_process(cloudApiApp)
  logs_app_process <- webfakes::new_app_process(logs_app)

  service <- parseHttpUrl(app$url())
  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    username = "some-user",
    accountId = "123",
    accessToken = "current-token",
    refreshToken = "refresh-token"
  )

  # Mock connectCloudUrls and connectCloudLogsClient
  local_mocked_bindings(
    connectCloudUrls = function() {
      list(logs = logs_app_process$url(), ui = "https://connect.posit.cloud")
    },
    connectCloudLogsClient = function() {
      list(
        getLogs = function(logChannel, authToken) {
          logsUrl <- logs_app_process$url()
          service <- parseHttpUrl(paste0(logsUrl, "/v1"))

          authInfo <- list(
            accessToken = authToken
          )

          path <- paste0(
            "/logs/",
            logChannel,
            "?traversal_direction=backward&limit=1500"
          )
          response <- GET(service, authInfo, path)
          response
        }
      )
    }
  )

  client <- connectCloudClient(service, authInfo)

  # Test failure case with logs - capture stderr output
  stderr_output <- capture.output(
    {
      result <- client$awaitCompletion("rev456")
    },
    type = "message"
  )

  # Check the result object
  expect_false(result$success)
  expect_equal(
    result$url,
    "https://connect.posit.cloud/some-user/content/content789"
  )
  expect_equal(result$error, "Deployment failed due to missing dependencies")

  # Check that logs were printed to stderr
  stderr_text <- paste(stderr_output, collapse = "\n")
  info <- paste0("stderr_text was:\n", stderr_text)

  expect_match(stderr_text, "Begin Publishing Log")
  expect_match(stderr_text, "End Publishing Log")
  expect_match(stderr_text, "Starting deployment...")
  expect_match(stderr_text, "Your app is busted!!")
  expect_match(stderr_text, "INFO:")
  expect_match(stderr_text, "ERROR:")
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
    name = "some-user",
    username = "some-user",
    accountId = "123",
    accessToken = "current-token",
    refreshToken = "refresh-token"
  )
  client <- connectCloudClient(service, authInfo)

  result <- client$withTokenRefreshRetry(
    mock_request_fn,
    "/test"
  )

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

  # Mock cloudAuthClient and registerAccount
  register_called <- FALSE
  local_mocked_bindings(
    cloudAuthClient = function() {
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
    },
    registerAccount = function(
      server,
      name,
      accountId,
      accessToken,
      refreshToken
    ) {
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
    accessToken = "current-token",
    refreshToken = "refresh-token"
  )
  client <- connectCloudClient(service, authInfo)

  result <- client$withTokenRefreshRetry(mock_request_fn, "/test")

  expect_equal(result$success, TRUE)
  expect_equal(result$data, "success after refresh")
  expect_equal(call_count, 2)
  expect_true(register_called)
})
