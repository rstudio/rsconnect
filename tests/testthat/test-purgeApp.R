test_that("purgeApp() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    purgeApp("myapp", account = "connect-user", server = "connect-server"),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    purgeApp("myapp", account = "cloud-user", server = "connect.posit.cloud"),
    regexp = "`server` must be shinyapps\\.io"
  )
})
