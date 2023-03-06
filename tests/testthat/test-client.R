test_that("cloud accounts create cloud clients", {
  account <- list(server = "rstudio.cloud")
  client <- clientForAccount(account)
  expect_equal(client$service(), "posit.cloud")

  account <- list(server = "posit.cloud")
  client <- clientForAccount(account)
  expect_equal(client$service(), "posit.cloud")
})

test_that("shinyapps accounts create shinyapps clients", {
  account <- list(server = "shinyapps.io")
  client <- clientForAccount(account)
  expect_equal(client$service(), "shinyapps.io")
})
