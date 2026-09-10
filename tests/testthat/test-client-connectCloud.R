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
  revision_app$get("/contents/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(id = I(req$params$id), state = "active", account_id = "acct-1"),
      auto_unbox = TRUE
    )
  })
  revision_app$get("/accounts", function(req, res) {
    res$set_status(200L)$send_json(
      list(data = list(list(id = "acct-1", name = "some-user")), total = 1),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(revision_app)
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

test_that("awaitCompletion falls back to an empty url instead of erroring when the account can't be resolved", {
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
  revision_app$get("/contents/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        state = "active",
        account_id = "acct-unknown"
      ),
      auto_unbox = TRUE
    )
  })
  revision_app$get("/accounts", function(req, res) {
    # "acct-unknown" is not in this list -- connectCloudContentUrl() can't
    # resolve it and would normally abort.
    res$set_status(200L)$send_json(
      list(data = list(list(id = "acct-1", name = "some-user")), total = 1),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(revision_app)
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

  # The unresolvable account must not crash the whole call -- the actual
  # publish result (success, in this case) still needs to come through.
  result <- expect_no_error(client$awaitCompletion("rev123"))
  expect_true(result$success)
  expect_equal(result$url, "")
  expect_null(result$error)
})

test_that("awaitCompletion shows a specific message when content was deleted right after publishing", {
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
  revision_app$get("/contents/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(id = I(req$params$id), state = "deleted"),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(revision_app)
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

  expect_message(
    result <- client$awaitCompletion("rev123"),
    "could not be found immediately after publishing"
  )
  expect_true(result$success)
  expect_equal(result$url, "")
})

test_that("getAccounts() paginates through multiple pages", {
  skip_if_not_installed("webfakes")

  accounts_app <- webfakes::new_app()
  accounts_app$use(webfakes::mw_json())
  accounts_app$get("/accounts", function(req, res) {
    allAccounts <- list(
      list(id = "acct-1", name = "account-one"),
      list(id = "acct-2", name = "account-two"),
      list(id = "acct-3", name = "account-three")
    )
    offset <- as.integer(req$query$offset)
    remaining <- allAccounts[seq(offset + 1, length(allAccounts))]
    page <- remaining[seq_len(min(2, length(remaining)))]
    res$set_status(200L)$send_json(
      list(data = page, total = length(allAccounts)),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(accounts_app)
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

  # 3 accounts, 2 per page -- must take two requests (offset=0, offset=2) to
  # accumulate all of them.
  result <- client$getAccounts()
  expect_equal(
    vapply(result$data, function(a) a$id, character(1)),
    c("acct-1", "acct-2", "acct-3")
  )
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
  revision_app$get("/contents/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(id = I(req$params$id), state = "active", account_id = "acct-1"),
      auto_unbox = TRUE
    )
  })
  revision_app$get("/accounts", function(req, res) {
    res$set_status(200L)$send_json(
      list(data = list(list(id = "acct-1", name = "some-user")), total = 1),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(revision_app)
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
  cloudApiApp$get("/contents/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(id = I(req$params$id), state = "active", account_id = "acct-1"),
      auto_unbox = TRUE
    )
  })
  cloudApiApp$get("/accounts", function(req, res) {
    res$set_status(200L)$send_json(
      list(data = list(list(id = "acct-1", name = "some-user")), total = 1),
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
  app <- webfakes::local_app_process(cloudApiApp)
  logs_app_process <- webfakes::local_app_process(logs_app)

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
      refreshToken,
      clientId,
      clientSecret
    ) {
      register_called <<- TRUE
      expect_equal(server, "connect.posit.cloud")
      expect_equal(name, "test-user")
      expect_equal(accountId, "123")
      expect_equal(accessToken, "new-access-token")
      expect_equal(refreshToken, "new-refresh-token")
      expect_null(clientId)
      expect_null(clientSecret)
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

test_that("withTokenRefreshRetry uses client_credentials when clientSecret is set", {
  call_count <- 0
  mock_request_fn <- function(service, authInfo, path) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      err <- structure(
        list(message = "HTTP 401"),
        class = c("rsconnect_http_401", "rsconnect_http", "error", "condition")
      )
      stop(err)
    } else {
      list(success = TRUE, data = "success after client_credentials refresh")
    }
  }

  register_called <- FALSE
  local_mocked_bindings(
    cloudAuthClient = function() {
      list(
        exchangeToken = function(request) {
          fail(
            "exchangeToken should not be called when clientSecret is present"
          )
        },
        exchangeClientCredentials = function(clientId, clientSecret) {
          expect_equal(clientId, "client-id-1")
          expect_equal(clientSecret, "client-secret-1")
          # RFC 6749 §4.4.3: no refresh_token expected in the response.
          list(access_token = "new-access-token", refresh_token = NULL)
        }
      )
    },
    registerAccount = function(
      server,
      name,
      accountId,
      accessToken,
      refreshToken,
      clientId,
      clientSecret
    ) {
      register_called <<- TRUE
      expect_equal(accessToken, "new-access-token")
      expect_null(refreshToken)
      # clientId/clientSecret must be re-persisted because registerAccount
      # rewrites the whole DCF (no merge with existing fields).
      expect_equal(clientId, "client-id-1")
      expect_equal(clientSecret, "client-secret-1")
    }
  )

  service <- list(host = "example.com", port = 443, protocol = "https")
  authInfo <- list(
    server = "connect.posit.cloud",
    name = "test-user",
    accountId = "123",
    accessToken = "current-token",
    clientId = "client-id-1",
    clientSecret = "client-secret-1"
  )
  client <- connectCloudClient(service, authInfo)

  result <- client$withTokenRefreshRetry(mock_request_fn, "/test")

  expect_equal(result$success, TRUE)
  expect_equal(call_count, 2)
  expect_true(register_called)
})

test_that("listApplications() paginates through multiple pages", {
  skip_if_not_installed("webfakes")

  allContents <- list(
    list(id = "c1", title = "app-one"),
    list(id = "c2", title = "app-two"),
    list(id = "c3", title = "app-three")
  )

  contents_app <- webfakes::new_app()
  contents_app$use(webfakes::mw_json())
  # Serve at most 2 items per page regardless of the client's limit param,
  # forcing two GET /contents requests to accumulate all 3 items.
  contents_app$get("/contents", function(req, res) {
    offset <- as.integer(req$query$offset)
    remaining <- allContents[seq(offset + 1L, length(allContents))]
    page <- remaining[seq_len(min(2L, length(remaining)))]
    res$set_status(200L)$send_json(
      list(data = page, total = length(allContents)),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(contents_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    username = "some-user",
    accountId = "acct-1",
    accessToken = "current-token",
    refreshToken = "refresh-token"
  )
  client <- connectCloudClient(service, authInfo)

  result <- client$listApplications("acct-1")
  expect_equal(length(result), 3L)
  expect_equal(
    vapply(result, function(x) x$id, character(1)),
    c("c1", "c2", "c3")
  )
  # name must be derived from title
  expect_equal(
    vapply(result, function(x) x$name, character(1)),
    c("app-one", "app-two", "app-three")
  )
})

test_that("listApplications() filters by exact name, not substring", {
  skip_if_not_installed("webfakes")

  allContents <- list(
    list(id = "c1", title = "my-app"),
    list(id = "c2", title = "my-app-extra"),
    list(id = "c3", title = "other-app")
  )

  contents_app <- webfakes::new_app()
  contents_app$use(webfakes::mw_json())
  contents_app$get("/contents", function(req, res) {
    res$set_status(200L)$send_json(
      list(data = allContents, total = length(allContents)),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(contents_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    username = "some-user",
    accountId = "acct-1",
    accessToken = "current-token",
    refreshToken = "refresh-token"
  )
  client <- connectCloudClient(service, authInfo)

  # "my-app-extra" shares a prefix with "my-app"; exact match must exclude it.
  result <- client$listApplications("acct-1", filters = list(name = "my-app"))
  expect_equal(length(result), 1L)
  expect_equal(result[[1]]$id, "c1")
  expect_equal(result[[1]]$name, "my-app")
})

test_that("listApplications() requests a stable sort and drops deleted content", {
  skip_if_not_installed("webfakes")

  contents_app <- webfakes::new_app()
  contents_app$use(webfakes::mw_json())
  contents_app$get("/contents", function(req, res) {
    # Reject if the stable-sort param is absent (paging regression guard).
    if (!identical(req$query$order_by, "created_time")) {
      res$set_status(400L)$send_json(
        list(error = "order_by=created_time must be present"),
        auto_unbox = TRUE
      )
      return()
    }
    res$set_status(200L)$send_json(
      list(
        data = list(
          list(id = "c1", title = "active-app", state = "active"),
          list(id = "c2", title = "deleted-app", state = "deleted")
        ),
        total = 2L
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(contents_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    username = "some-user",
    accountId = "acct-1",
    accessToken = "current-token",
    refreshToken = "refresh-token"
  )
  client <- connectCloudClient(service, authInfo)

  # order_by must have been sent (else the app 400s); deleted content is dropped.
  result <- client$listApplications("acct-1")
  expect_equal(length(result), 1L)
  expect_equal(result[[1]]$id, "c1")
  expect_equal(result[[1]]$name, "active-app")
})

test_that("listApplicationAuthorization GETs /contents/{id}/users and returns parsed data", {
  skip_if_not_installed("webfakes")

  users_app <- webfakes::new_app()
  users_app$use(webfakes::mw_json())
  users_app$get("/contents/:id/users", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        data = list(
          list(
            user = list(id = "user-uuid-1", email = "alice@example.com"),
            account = "acct-a"
          ),
          list(
            user = list(id = "user-uuid-2", email = "bob@example.com"),
            account = "acct-b"
          )
        )
      ),
      auto_unbox = TRUE
    )
  })
  app <- webfakes::local_app_process(users_app)
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

  result <- client$listApplicationAuthorization("content-abc")
  expect_equal(length(result), 2L)
  expect_equal(result[[1]]$user$email, "alice@example.com")
  expect_equal(result[[2]]$user$email, "bob@example.com")
  expect_equal(result[[1]]$user$id, "user-uuid-1")
})

test_that("removeApplicationUser DELETEs /contents/{id}/users/{userId} and returns TRUE", {
  skip_if_not_installed("webfakes")

  delete_app <- webfakes::new_app()
  delete_app$use(webfakes::mw_json())
  delete_app$delete("/contents/:id/users/:userId", function(req, res) {
    res$set_status(200L)$send_json(list(), auto_unbox = TRUE)
  })
  app <- webfakes::local_app_process(delete_app)
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

  result <- client$removeApplicationUser("content-abc", "user-uuid-1")
  expect_true(result)
})

test_that("inviteApplicationUser POSTs expected JSON fields to /contents/{id}/invitations", {
  skip_if_not_installed("webfakes")

  invite_app <- webfakes::new_app()
  invite_app$use(webfakes::mw_json())
  invite_app$post("/contents/:id/invitations", function(req, res) {
    j <- req$json
    # Validate required payload shape; 400 forces a client error if the body is wrong
    has_email_inv <- is.list(j$email_invitations) &&
      length(j$email_invitations) >= 1L
    has_addr <- identical(
      j$email_invitations[[1]]$email_address,
      "alice@example.com"
    )
    has_recv_inv <- is.list(j$recipient_invitations)
    has_message <- identical(j$message, "Welcome!")
    if (has_email_inv && has_addr && has_recv_inv && has_message) {
      res$set_status(200L)$send_json(list(), auto_unbox = TRUE)
    } else {
      res$set_status(400L)$send_json(
        list(error = "unexpected body shape"),
        auto_unbox = TRUE
      )
    }
  })
  app <- webfakes::local_app_process(invite_app)
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

  result <- client$inviteApplicationUser(
    "content-abc",
    "alice@example.com",
    TRUE,
    "Welcome!"
  )
  expect_true(result)
})

test_that("inviteApplicationUser sends null message field when emailMessage is NULL", {
  skip_if_not_installed("webfakes")

  null_msg_app <- webfakes::new_app()
  null_msg_app$use(webfakes::mw_json())
  null_msg_app$post("/contents/:id/invitations", function(req, res) {
    j <- req$json
    # toJSON(list(message = NULL), null = "null") renders {"message":null,...};
    # jsonlite parses null back to NULL, so is.null(j$message) must be TRUE.
    has_email_inv <- is.list(j$email_invitations) &&
      length(j$email_invitations) >= 1L
    has_addr <- identical(
      j$email_invitations[[1]]$email_address,
      "alice@example.com"
    )
    has_null_msg <- is.null(j$message) && "message" %in% names(j)
    if (has_email_inv && has_addr && has_null_msg) {
      res$set_status(200L)$send_json(list(), auto_unbox = TRUE)
    } else {
      res$set_status(400L)$send_json(
        list(error = "expected null message field"),
        auto_unbox = TRUE
      )
    }
  })
  app <- webfakes::local_app_process(null_msg_app)
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

  result <- client$inviteApplicationUser(
    "content-abc",
    "alice@example.com",
    TRUE,
    NULL
  )
  expect_true(result)
})

test_that("listApplicationInvitations GETs /contents/{id}/invitations?accepted_time__isnull=true", {
  skip_if_not_installed("webfakes")

  list_app <- webfakes::new_app()
  list_app$use(webfakes::mw_json())
  list_app$get("/contents/:id/invitations", function(req, res) {
    # 400 if the required filter query param is absent or wrong
    if (identical(req$query$accepted_time__isnull, "true")) {
      res$set_status(200L)$send_json(
        list(
          data = list(
            list(
              id = "inv-1",
              email_address = "bob@example.com",
              is_expired = FALSE
            )
          )
        ),
        auto_unbox = TRUE
      )
    } else {
      res$set_status(400L)$send_json(
        list(error = "accepted_time__isnull=true must be present"),
        auto_unbox = TRUE
      )
    }
  })
  app <- webfakes::local_app_process(list_app)
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

  result <- client$listApplicationInvitations("content-abc")

  expect_equal(length(result), 1L)
  expect_equal(result[[1]]$id, "inv-1")
  expect_equal(result[[1]]$email_address, "bob@example.com")
})

test_that("resendApplicationInvitation sends {} (object not array) to /content_invitations/{id}/resend", {
  skip_if_not_installed("webfakes")

  resend_app <- webfakes::new_app()
  resend_app$use(webfakes::mw_json())
  resend_app$post("/content_invitations/:id/resend", function(req, res) {
    j <- req$json
    # {} parses to a named empty list; [] parses to an unnamed empty list
    # 400 if setNames(list(), character(0)) somehow regressed to list()
    if (is.list(j) && length(j) == 0L && !is.null(names(j))) {
      res$set_status(200L)$send_json(list(), auto_unbox = TRUE)
    } else {
      res$set_status(400L)$send_json(
        list(error = "body must be JSON object {} not array []"),
        auto_unbox = TRUE
      )
    }
  })
  app <- webfakes::local_app_process(resend_app)
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

  result <- client$resendApplicationInvitation("inv-uuid-1", FALSE)
  expect_true(result)
})

test_that("listApplicationAuthorization accumulates multiple pages", {
  skip_if_not_installed("webfakes")

  # Page 1: 2 users. Page 2: 1 user. Total=3 reported on each page.
  page1 <- list(
    list(user = list(id = "u1", email = "a@example.com"), account = "acct-a"),
    list(user = list(id = "u2", email = "b@example.com"), account = "acct-b")
  )
  page2 <- list(
    list(user = list(id = "u3", email = "c@example.com"), account = "acct-c")
  )

  users_app <- webfakes::new_app()
  users_app$use(webfakes::mw_json())
  users_app$get("/contents/:id/users", function(req, res) {
    # Reject if the stable-sort param is absent (paging regression guard).
    if (!identical(req$query$order_by, "created_time")) {
      res$set_status(400L)$send_json(
        list(error = "order_by=created_time must be present"),
        auto_unbox = TRUE
      )
      return()
    }
    offset <- as.integer(req$query$offset %||% "0")
    if (offset == 0L) {
      res$set_status(200L)$send_json(
        list(data = page1, total = 3L),
        auto_unbox = TRUE
      )
    } else {
      res$set_status(200L)$send_json(
        list(data = page2, total = 3L),
        auto_unbox = TRUE
      )
    }
  })
  app <- webfakes::local_app_process(users_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    username = "some-user",
    accountId = "123",
    accessToken = "tok",
    refreshToken = "ref"
  )
  client <- connectCloudClient(service, authInfo)

  result <- client$listApplicationAuthorization("content-abc")
  expect_equal(length(result), 3L)
  expect_equal(result[[1]]$user$id, "u1")
  expect_equal(result[[2]]$user$id, "u2")
  expect_equal(result[[3]]$user$id, "u3")
})

test_that("listApplicationInvitations accumulates multiple pages and keeps accepted_time__isnull filter", {
  skip_if_not_installed("webfakes")

  page1 <- list(
    list(id = "inv-1", email_address = "a@example.com", is_expired = FALSE),
    list(id = "inv-2", email_address = "b@example.com", is_expired = FALSE)
  )
  page2 <- list(
    list(id = "inv-3", email_address = "c@example.com", is_expired = TRUE)
  )

  inv_app <- webfakes::new_app()
  inv_app$use(webfakes::mw_json())
  inv_app$get("/contents/:id/invitations", function(req, res) {
    # Reject if the required filter or stable-sort param is absent.
    if (
      !identical(req$query$accepted_time__isnull, "true") ||
        !identical(req$query$order_by, "created_time")
    ) {
      res$set_status(400L)$send_json(
        list(
          error = "accepted_time__isnull=true and order_by=created_time must be present"
        ),
        auto_unbox = TRUE
      )
      return()
    }
    offset <- as.integer(req$query$offset %||% "0")
    if (offset == 0L) {
      res$set_status(200L)$send_json(
        list(data = page1, total = 3L),
        auto_unbox = TRUE
      )
    } else {
      res$set_status(200L)$send_json(
        list(data = page2, total = 3L),
        auto_unbox = TRUE
      )
    }
  })
  app <- webfakes::local_app_process(inv_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    username = "some-user",
    accountId = "123",
    accessToken = "tok",
    refreshToken = "ref"
  )
  client <- connectCloudClient(service, authInfo)

  result <- client$listApplicationInvitations("content-abc")
  expect_equal(length(result), 3L)
  expect_equal(result[[1]]$id, "inv-1")
  expect_equal(result[[2]]$id, "inv-2")
  expect_equal(result[[3]]$id, "inv-3")
})

# --- envVars / secrets tests ---------------------------------------------------
# webfakes runs in a separate process so we can't use <<- to capture req$json.
# Instead the server validates the payload and returns 400 on a bad shape
# (the client will then throw), so a clean 200 response proves the assertion.

test_that("createContent sends empty secrets array when envVars is NULL", {
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/contents", function(req, res) {
    j <- req$json
    # secrets must be an empty JSON array; any other shape means Sys.getenv()
    # was called on the whole environment (pre-fix behaviour on R < 4.2.0).
    if (is.list(j$secrets) && length(j$secrets) == 0L) {
      res$set_status(200L)$send_json(
        list(id = "content-new-1"),
        auto_unbox = TRUE
      )
    } else {
      res$set_status(400L)$send_json(
        list(error = "secrets must be an empty array"),
        auto_unbox = TRUE
      )
    }
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    accountId = "acct-1",
    accessToken = "tok",
    refreshToken = "ref"
  )
  client <- connectCloudClient(service, authInfo)

  expect_no_error(
    client$createContent(
      name = "my-app",
      title = "My App",
      accountId = "acct-1",
      appMode = "shiny",
      primaryFile = "app.R",
      envVars = NULL
    )
  )
})

test_that("createContent sends empty secrets array when envVars is character(0)", {
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/contents", function(req, res) {
    j <- req$json
    if (is.list(j$secrets) && length(j$secrets) == 0L) {
      res$set_status(200L)$send_json(
        list(id = "content-new-2"),
        auto_unbox = TRUE
      )
    } else {
      res$set_status(400L)$send_json(
        list(error = "secrets must be an empty array"),
        auto_unbox = TRUE
      )
    }
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    accountId = "acct-1",
    accessToken = "tok",
    refreshToken = "ref"
  )
  client <- connectCloudClient(service, authInfo)

  expect_no_error(
    client$createContent(
      name = "my-app",
      title = "My App",
      accountId = "acct-1",
      appMode = "shiny",
      primaryFile = "app.R",
      envVars = character(0)
    )
  )
})

test_that("createContent includes envVar name and value in secrets when envVars is non-empty", {
  skip_if_not_installed("webfakes")
  skip_if_not_installed("withr")

  withr::local_envvar(RSCONNECT_TEST_CC_KEY = "test-secret-value")

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/contents", function(req, res) {
    j <- req$json
    ok <- is.list(j$secrets) &&
      length(j$secrets) == 1L &&
      identical(j$secrets[[1]]$name, "RSCONNECT_TEST_CC_KEY") &&
      identical(j$secrets[[1]]$value, "test-secret-value")
    if (ok) {
      res$set_status(200L)$send_json(
        list(id = "content-new-3"),
        auto_unbox = TRUE
      )
    } else {
      res$set_status(400L)$send_json(
        list(error = "expected one secret with correct name/value"),
        auto_unbox = TRUE
      )
    }
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    accountId = "acct-1",
    accessToken = "tok",
    refreshToken = "ref"
  )
  client <- connectCloudClient(service, authInfo)

  expect_no_error(
    client$createContent(
      name = "my-app",
      title = "My App",
      accountId = "acct-1",
      appMode = "shiny",
      primaryFile = "app.R",
      envVars = "RSCONNECT_TEST_CC_KEY"
    )
  )
})

test_that("updateContent sends empty secrets array when envVars is NULL", {
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$patch("/contents/:id", function(req, res) {
    j <- req$json
    if (is.list(j$secrets) && length(j$secrets) == 0L) {
      res$set_status(200L)$send_json(
        list(id = I(req$params$id)),
        auto_unbox = TRUE
      )
    } else {
      res$set_status(400L)$send_json(
        list(error = "secrets must be an empty array"),
        auto_unbox = TRUE
      )
    }
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    accountId = "acct-1",
    accessToken = "tok",
    refreshToken = "ref"
  )
  client <- connectCloudClient(service, authInfo)

  expect_no_error(
    client$updateContent(
      contentId = "content-abc",
      envVars = NULL,
      newBundle = FALSE,
      primaryFile = "app.R",
      appMode = "shiny"
    )
  )
})

test_that("updateContent includes envVar name and value in secrets when envVars is non-empty", {
  skip_if_not_installed("webfakes")
  skip_if_not_installed("withr")

  withr::local_envvar(RSCONNECT_TEST_CC_KEY = "test-secret-value")

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$patch("/contents/:id", function(req, res) {
    j <- req$json
    ok <- is.list(j$secrets) &&
      length(j$secrets) == 1L &&
      identical(j$secrets[[1]]$name, "RSCONNECT_TEST_CC_KEY") &&
      identical(j$secrets[[1]]$value, "test-secret-value")
    if (ok) {
      res$set_status(200L)$send_json(
        list(id = I(req$params$id)),
        auto_unbox = TRUE
      )
    } else {
      res$set_status(400L)$send_json(
        list(error = "expected one secret with correct name/value"),
        auto_unbox = TRUE
      )
    }
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(
    server = "connect.posit.cloud",
    name = "some-user",
    accountId = "acct-1",
    accessToken = "tok",
    refreshToken = "ref"
  )
  client <- connectCloudClient(service, authInfo)

  expect_no_error(
    client$updateContent(
      contentId = "content-abc",
      envVars = "RSCONNECT_TEST_CC_KEY",
      newBundle = FALSE,
      primaryFile = "app.R",
      appMode = "shiny"
    )
  )
})
