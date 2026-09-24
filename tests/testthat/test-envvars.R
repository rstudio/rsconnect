test_that("listAccountEnvVars() aborts for shinyapps.io and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    listAccountEnvVars(account = "shinyapps-user", server = "shinyapps.io"),
    regexp = "does not support environment variables"
  )
  expect_error(
    listAccountEnvVars(account = "cloud-user", server = "connect.posit.cloud"),
    regexp = "does not support environment variables"
  )
})

test_that("updateAccountEnvVars() aborts for shinyapps.io and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    updateAccountEnvVars(
      "FOO",
      account = "shinyapps-user",
      server = "shinyapps.io"
    ),
    regexp = "does not support environment variables"
  )
  expect_error(
    updateAccountEnvVars(
      "FOO",
      account = "cloud-user",
      server = "connect.posit.cloud"
    ),
    regexp = "does not support environment variables"
  )
})

test_that("env var errors name the server and the calling function", {
  local_mocked_account_info()

  err <- expect_error(
    listAccountEnvVars(account = "cloud-user", server = "connect.posit.cloud"),
    regexp = "Posit Connect Cloud does not support environment variables"
  )
  expect_equal(rlang::call_name(err$call), "listAccountEnvVars")
})
