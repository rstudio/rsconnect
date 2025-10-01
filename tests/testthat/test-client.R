test_that("shinyapps accounts create shinyapps clients", {
  account <- list(server = "shinyapps.io")
  client <- clientForAccount(account)
  expect_equal(client$service(), "shinyapps.io")
})

test_that("connect cloud accounts create connect cloud clients", {
  account <- list(server = "connect.posit.cloud")
  client <- clientForAccount(account)
  expect_equal(client$service(), "connect.posit.cloud")
})

test_that("connect accounts create connect clients", {
  local_temp_config()

  addTestServer("example.com")
  account <- list(server = "example.com")
  client <- clientForAccount(account)
  expect_equal(client$service(), "connect")
})
