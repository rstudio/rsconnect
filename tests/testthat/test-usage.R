test_that("showUsage() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    showUsage(account = "connect-user", server = "connect-server"),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    showUsage(account = "cloud-user", server = "connect.posit.cloud"),
    regexp = "`server` must be shinyapps\\.io"
  )
})

test_that("accountUsage() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    accountUsage(account = "connect-user", server = "connect-server"),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    accountUsage(account = "cloud-user", server = "connect.posit.cloud"),
    regexp = "`server` must be shinyapps\\.io"
  )
})
