test_that("tasks() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    tasks(account = "connect-user", server = "connect-server"),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    tasks(account = "cloud-user", server = "connect.posit.cloud"),
    regexp = "`server` must be shinyapps\\.io"
  )
})

test_that("taskLog() aborts for Posit Connect and Connect Cloud accounts", {
  local_mocked_account_info()

  expect_error(
    taskLog(1, account = "connect-user", server = "connect-server"),
    regexp = "`server` must be shinyapps\\.io"
  )
  expect_error(
    taskLog(1, account = "cloud-user", server = "connect.posit.cloud"),
    regexp = "`server` must be shinyapps\\.io"
  )
})
