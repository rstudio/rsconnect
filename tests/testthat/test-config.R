test_that("account file returned with server name", {
  local_temp_config()
  registerUserApiKey("simple", "alice", 13, "alice-api-key")

  expected <- normalizePath(file.path(
    rsconnectConfigDir("accounts"),
    "simple/alice.dcf"
  ))
  dir <- accountConfigFile("alice", server = "simple")
  expect_equal(dir, expected)
})

test_that("account file containing pattern characters found with server name", {
  local_temp_config()
  registerUserApiKey("complex", "hatter+mad@example.com", 42, "hatter-api-key")

  # https://github.com/rstudio/rsconnect/issues/620
  expected <- normalizePath(file.path(
    rsconnectConfigDir("accounts"),
    "complex/hatter+mad@example.com.dcf"
  ))
  dir <- accountConfigFile("hatter+mad@example.com", server = "complex")
  expect_equal(dir, expected)
})
