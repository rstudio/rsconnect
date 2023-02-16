setupFakeAccounts <- function() {
  registerUserApiKey("simple", "alice", 13, "alice-api-key")
  registerUserApiKey("complex", "hatter+mad@example.com", 42, "hatter-api-key")
}

test_that("account file returned with server name", {
  local_temp_config()
  setupFakeAccounts()

  expected <- normalizePath(file.path(
    rsconnectConfigDir("accounts"),
    "simple/alice.dcf"
  ))
  dir <- accountConfigFile("alice", server = "simple")
  expect_equal(dir, expected)
})

test_that("account file containing pattern characters found with server name", {
  local_temp_config()
  setupFakeAccounts()

  # https://github.com/rstudio/rsconnect/issues/620
  expected <- normalizePath(file.path(
    rsconnectConfigDir("accounts"),
    "complex/hatter+mad@example.com.dcf"
  ))
  dir <- accountConfigFile("hatter+mad@example.com", server = "complex")
  expect_equal(dir, expected)
})

test_that("account file found without server name", {
  local_temp_config()
  setupFakeAccounts()

  expected <- normalizePath(file.path(
    rsconnectConfigDir("accounts"),
    "simple/alice.dcf"
  ))
  dir <- accountConfigFile("alice", server = NULL)
  expect_equal(dir, expected)
})

test_that("account file containing pattern characters found without server name", {
  local_temp_config()
  setupFakeAccounts()

  # https://github.com/rstudio/rsconnect/issues/620
  expected <- normalizePath(file.path(
    rsconnectConfigDir("accounts"),
    "complex/hatter+mad@example.com.dcf"
  ))
  dir <- accountConfigFile("hatter+mad@example.com", server = NULL)
  expect_equal(dir, expected)
})

test_that("All hosted product names are identified as cloud", {
  expect_true(isCloudServer("shinyapps.io"))
  expect_true(isCloudServer("rstudio.cloud"))
  expect_true(isCloudServer("posit.cloud"))
  expect_false(isCloudServer("connect.internal"))
})

test_that("secrets are hidden from casual inspection", {
  local_temp_config()
  registerUserToken("example.com", "john", "userId", "token", "THIS IS A SECRET")
  registerCloudTokenSecret(
    "shinyapps.io",
    "susan",
    "userId",
    "accountId",
    "token",
    "THIS IS A SECRET"
  )

  expect_snapshot({
    accountInfo("john")$private_key
    accountInfo("susan")$secret

    str(accountInfo("john"))
  })

})
