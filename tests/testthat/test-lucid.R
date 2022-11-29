context("lucid")

test_that("cloud accounts create cloud clients", {
  account <- list(
    server = "rstudio.cloud"
  )

  client <- lucidClientForAccount(account)
  expect_equal(client$service(), "posit.cloud")

  account <- list(
    server = "posit.cloud"
  )

  client <- lucidClientForAccount(account)
  expect_equal(client$service(), "posit.cloud")
})

test_that("shinyapps accounts create shinyapps clients", {
  account <- list(
    server = "shinyapps.io"
  )

  client <- lucidClientForAccount(account)
  expect_equal(client$service(), "shinyapps.io")
})
