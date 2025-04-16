test_that("no accounts returns empty data frame", {
  local_temp_config()

  expect_equal(
    accounts(),
    data.frame(
      name = character(),
      server = character(),
      stringsAsFactors = FALSE
    )
  )
})

test_that("hasAccounts works", {
  local_temp_config()
  addTestServer()
  addTestAccount("john")

  expect_true(hasAccount("john", "example.com"))
  expect_false(hasAccount("john", "example2.com"))
  expect_false(hasAccount("mary", "example.com"))
})

test_that("secrets are hidden from casual inspection", {
  local_temp_config()
  registerAccount("server", "1", "id", token = "token", secret = "SECRET")
  registerAccount("server", "2", "id", token = "token", private_key = "SECRET")
  registerAccount("server", "3", "id", apiKey = "SECRET")

  expect_snapshot({
    accountInfo("1")$secret
    accountInfo("2")$private_key
    accountInfo("3")$apiKey
  })
})

test_that("setAccountInfo() gives nice error on bad copy and paste", {
  expect_snapshot(setAccountInfo("name", "token", "<SECRET>"), error = TRUE)
})

test_that("accountInfo() returns account information", {
  local_temp_config()
  addTestServer()
  addTestAccount("john")

  accountDetails <- accountInfo("john", "example.com")
  expect_equal(accountDetails$name, "john")
  expect_equal(accountDetails$username, "john")
  expect_equal(accountDetails$server, "example.com")
})

test_that("accountInfo() returns account information", {
  local_temp_config()
  addTestServer()
  addTestAccount("john")

  accountDetails <- accountInfo("john", "example.com")
  expect_equal(accountDetails$name, "john")
  # username is included for backwards compatibility. (#1024)
  expect_equal(accountDetails$username, "john")
  expect_equal(accountDetails$server, "example.com")
})

test_that("accountInfo() returns pre-rsconnect-1.0.0 account information", {
  local_temp_config()
  addTestServer()

  # Subset of rsconnect-0.8.29 account fields.
  fields <- list(
    username = "john",
    server = "example.com",
    accountId = "john"
  )

  path <- accountConfigFile("john", "example.com")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.dcf(compact(fields), path, width = 100)

  accountDetails <- accountInfo("john", "example.com")
  # name copied from username, as "name" is the current field name.
  expect_equal(accountDetails$name, "john")
  # username retained for backwards compatibility.
  expect_equal(accountDetails$username, "john")
  expect_equal(accountDetails$server, "example.com")
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

test_that("findAccountInfo redacts snowflakeToken", {
  local_temp_config()

  # Create mock account info with snowflakeToken
  fields <- list(
    name = "testuser",
    server = "example.com",
    accountId = "user123",
    snowflakeToken = "sensitive_token_data"
  )

  path <- accountConfigFile("testuser", "example.com")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.dcf(compact(fields), path, width = 100)

  # Get account info and check that snowflakeToken is redacted
  info <- findAccountInfo("testuser", "example.com")
  expect_s3_class(info$snowflakeToken, "rsconnect_secret")
})
