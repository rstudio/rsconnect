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
  authInfo <- list(
    snowflakeToken = list(
      Authorization = "Snowflake Token=\"mock_token\"",
      `X-Custom-Header` = "custom-value"
    )
  )

  headers <- authHeaders(authInfo, "GET", "/path")

  expect_equal(headers$Authorization, "Snowflake Token=\"mock_token\"")
  expect_equal(headers$`X-Custom-Header`, "custom-value")
})

test_that("authHeaders handles snowflakeToken with API key", {
  authInfo <- list(
    apiKey = secret("the-api-key"),
    snowflakeToken = list(
      Authorization = "Snowflake Token=\"mock_token\"",
      `X-Custom-Header` = "custom-value"
    )
  )

  # Test authHeaders
  headers <- authHeaders(authInfo, "GET", "/path")

  # Verify snowflakeToken headers were used
  expect_equal(headers$Authorization, "Snowflake Token=\"mock_token\"")
  expect_equal(headers$`X-RSC-Authorization`, "Key the-api-key")
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

test_that("registerAccount stores both apiKey and snowflakeConnectionName for SPCS accounts", {
  local_temp_config()

  # Register an SPCS account with both apiKey and snowflakeConnectionName
  registerAccount(
    serverName = "spcs.example.com",
    accountName = "spcsuser",
    accountId = "user456",
    apiKey = "test-api-key-789",
    snowflakeConnectionName = "spcs_connection"
  )

  # Check the account info has both fields
  info <- accountInfo("spcsuser", "spcs.example.com")
  expect_equal(info$snowflakeConnectionName, "spcs_connection")
  expect_equal(as.character(info$apiKey), "test-api-key-789")
})
