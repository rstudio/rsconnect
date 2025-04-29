test_that("isSPCSServer correctly identifies Snowpark Container Services servers", {
  local_temp_config()

  # Register a server with a snowflakecomputing.app URL
  addTestServer(
    url = "https://test-abc123.snowflakecomputing.app/__api__",
    name = "spcs_server"
  )

  # Mock serverInfo to return the URL for our test
  local_mocked_bindings(
    serverInfo = function(server) {
      if (server == "spcs_server") {
        return(list(url = "https://test-abc123.snowflakecomputing.app/__api__"))
      }
      list(url = "https://example.com")
    }
  )

  expect_true(isSPCSServer("spcs_server"))
  expect_false(isSPCSServer("example.com"))
})

test_that("authHeaders handles snowflakeToken", {
  # mock authInfo with snowflakeToken
  authInfo <- list(
    snowflakeToken = list(
      Authorization = "Snowflake Token=\"mock_token\"",
      `X-Custom-Header` = "custom-value"
    )
  )

  headers <- authHeaders(authInfo, "GET", "/path")

  # Verify snowflakeToken headers were used
  expect_equal(headers$Authorization, "Snowflake Token=\"mock_token\"")
  expect_equal(headers$`X-Custom-Header`, "custom-value")
})

test_that("registerAccount stores snowflakeConnectionName", {
  local_temp_config()

  # Register an account with snowflakeConnectionName
  registerAccount(
    serverName = "example.com",
    accountName = "testuser",
    accountId = "user123",
    snowflakeConnectionName = "test_connection"
  )

  # Check the account info has the snowflakeConnectionName
  info <- accountInfo("testuser", "example.com")
  expect_equal(info$snowflakeConnectionName, "test_connection")
})
