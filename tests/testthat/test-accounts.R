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

test_that("connectSPCSUser accepts NULL snowflakeConnectionName and gets default", {
  local_temp_config()

  local_mocked_bindings(
    findServer = function(server) "test_server",
    checkConnectServer = function(server) invisible(),
    serverInfo = function(server) {
      list(url = "https://prefix-account.snowflakecomputing.app/__api__")
    },
    getDefaultSnowflakeConnectionName = function(url) {
      "default"
    },
    getSPCSAuthedUser = function(server, apiKey, snowflakeConnectionName) {
      # Verify we have the default connection name.
      expect_equal(snowflakeConnectionName, "default")
      list(id = "user123", username = "testuser")
    }
  )

  expect_no_error(
    connectSPCSUser(
      account = "testuser",
      server = "test_server",
      apiKey = "test_api_key",
      snowflakeConnectionName = NULL,
      quiet = TRUE
    )
  )
})

test_that("connectSPCSUser works with explicit snowflakeConnectionName", {
  local_temp_config()

  local_mocked_bindings(
    findServer = function(server) "test_server",
    checkConnectServer = function(server) invisible(),
    serverInfo = function(server) {
      list(url = "https://prefix-account.snowflakecomputing.app/__api__")
    },
    getSPCSAuthedUser = function(server, apiKey, snowflakeConnectionName) {
      # Verify we have the correct connection name.
      expect_equal(snowflakeConnectionName, "test_connection")
      list(id = "user123", username = "testuser")
    }
  )

  expect_no_error(
    connectSPCSUser(
      account = "testuser",
      server = "test_server",
      apiKey = "test_api_key",
      snowflakeConnectionName = "test_connection",
      quiet = TRUE
    )
  )
})

test_that("registerAccount stores clientId and clientSecret", {
  local_temp_config()

  registerAccount(
    serverName = "connect.posit.cloud",
    accountName = "ci",
    accountId = "acc-1",
    accessToken = "access",
    clientId = "client-1",
    clientSecret = "secret-1"
  )

  info <- accountInfo("ci", "connect.posit.cloud")
  expect_equal(info$clientId, "client-1")
  # clientSecret is wrapped via secret() so that casual printing redacts it.
  expect_s3_class(info$clientSecret, "rsconnect_secret")
})

# Builds a fake Connect Cloud client whose getAccounts() returns the supplied
# list. Used to drive connectCloudClientCredentials in tests without hitting the
# network.
fakeCloudClient <- function(accounts) {
  list(
    getAccounts = function() list(data = accounts)
  )
}

# Mocks the OAuth exchange that connectCloudClientCredentials performs before
# touching the API.
mockClientCredentialsAuth <- function(
  accessToken = "access-token",
  refreshToken = NULL
) {
  function() {
    list(
      exchangeClientCredentials = function(clientId, clientSecret) {
        list(access_token = accessToken, refresh_token = refreshToken)
      }
    )
  }
}

test_that("connectCloudClientCredentials auto-picks the only publishable account", {
  local_temp_config()

  registered <- NULL
  local_mocked_bindings(
    cloudAuthClient = mockClientCredentialsAuth(),
    connectCloudClient = function(service, authInfo) {
      fakeCloudClient(list(
        list(id = "acc-1", name = "alice", permissions = list("content:create"))
      ))
    },
    registerAccount = function(...) {
      registered <<- list(...)
    }
  )

  connectCloudClientCredentials(
    clientId = "client-1",
    clientSecret = "secret-1",
    quiet = TRUE
  )

  expect_equal(registered$serverName, "connect.posit.cloud")
  expect_equal(registered$accountName, "alice")
  expect_equal(registered$accountId, "acc-1")
  expect_equal(registered$accessToken, "access-token")
  expect_equal(registered$clientId, "client-1")
  expect_equal(registered$clientSecret, "secret-1")
})

test_that("connectCloudClientCredentials accepts an explicit accountId", {
  local_temp_config()

  registered <- NULL
  local_mocked_bindings(
    cloudAuthClient = mockClientCredentialsAuth(),
    connectCloudClient = function(service, authInfo) {
      fakeCloudClient(list(
        list(
          id = "acc-1",
          name = "alice",
          permissions = list("content:create")
        ),
        list(id = "acc-2", name = "bob", permissions = list("content:create"))
      ))
    },
    registerAccount = function(...) {
      registered <<- list(...)
    }
  )

  connectCloudClientCredentials(
    clientId = "client-1",
    clientSecret = "secret-1",
    accountId = "acc-2",
    quiet = TRUE
  )

  expect_equal(registered$accountName, "bob")
  expect_equal(registered$accountId, "acc-2")
})

test_that("connectCloudClientCredentials errors when accountId is unknown", {
  local_temp_config()

  local_mocked_bindings(
    cloudAuthClient = mockClientCredentialsAuth(),
    connectCloudClient = function(service, authInfo) {
      fakeCloudClient(list(
        list(id = "acc-1", name = "alice", permissions = list("content:create"))
      ))
    }
  )

  expect_error(
    connectCloudClientCredentials(
      clientId = "client-1",
      clientSecret = "secret-1",
      accountId = "acc-missing",
      quiet = TRUE
    ),
    "not found"
  )
})

test_that("connectCloudClientCredentials distinguishes unpublishable accounts in errors", {
  local_temp_config()

  local_mocked_bindings(
    cloudAuthClient = mockClientCredentialsAuth(),
    connectCloudClient = function(service, authInfo) {
      # acc-2 exists but the credentials lack content:create on it.
      fakeCloudClient(list(
        list(
          id = "acc-1",
          name = "alice",
          permissions = list("content:create")
        ),
        list(id = "acc-2", name = "bob", permissions = list())
      ))
    }
  )

  expect_error(
    connectCloudClientCredentials(
      clientId = "client-1",
      clientSecret = "secret-1",
      accountId = "acc-2",
      quiet = TRUE
    ),
    "does not grant publish permission"
  )
})

test_that("connectCloudClientCredentials prompts when multiple publishable accounts are available", {
  local_temp_config()

  registered <- NULL
  local_mocked_bindings(
    cloudAuthClient = mockClientCredentialsAuth(),
    connectCloudClient = function(service, authInfo) {
      fakeCloudClient(list(
        list(
          id = "acc-1",
          name = "alice",
          permissions = list("content:create")
        ),
        list(id = "acc-2", name = "bob", permissions = list("content:create"))
      ))
    },
    registerAccount = function(...) {
      registered <<- list(...)
    }
  )

  simulate_user_input(2)
  connectCloudClientCredentials(
    clientId = "client-1",
    clientSecret = "secret-1",
    quiet = TRUE
  )

  expect_equal(registered$accountId, "acc-2")
  expect_equal(registered$accountName, "bob")
})

test_that("getSPCSAuthedUser passes snowflakeConnectionName to clientForAccount", {
  local_temp_config()

  local_mocked_bindings(
    serverInfo = function(server) {
      list(url = "https://prefix-account.snowflakecomputing.app/__api__")
    },
    clientForAccount = function(account) {
      # Check that snowflakeConnectionName is passed through
      expect_equal(account$snowflakeConnectionName, "test_connection")
      list(currentUser = function() list(id = "user123", username = "testuser"))
    }
  )

  result <- getSPCSAuthedUser(
    "test_server",
    "test_api_key",
    "test_connection"
  )

  expect_equal(result$username, "testuser")
})
