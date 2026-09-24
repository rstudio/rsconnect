test_that("restartApp() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    restartApp("myapp", account = "connect-user", server = "connect-server"),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    restartApp("myapp", account = "cloud-user", server = "connect.posit.cloud"),
    regexp = "`server` must be shinyapps\\.io"
  )
})
