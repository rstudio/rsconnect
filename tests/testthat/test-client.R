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

test_that("shinyapps getLogs does not include tail in query", {
  captured_query <- NULL
  local_mocked_bindings(
    GET = function(service, authInfo, path, query = NULL, ...) {
      captured_query <<- query
      "fake log output"
    }
  )

  client <- shinyAppsClient(service = list(), authInfo = list())
  client$getLogs("12345")
  cat(captured_query)
  expect_no_match(captured_query, "tail")
  expect_match(captured_query, "count=50")

  client$getLogs("12345", entries = 100)
  expect_no_match(captured_query, "tail")
  expect_match(captured_query, "count=100")

  client$getLogs("12345", format = "json")
  expect_no_match(captured_query, "tail")
  expect_match(captured_query, "count=50")
  expect_match(captured_query, "format=json")
})

test_that("connect accounts create connect clients", {
  local_temp_config()

  addTestServer("example.com")
  account <- list(server = "example.com")
  client <- clientForAccount(account)
  expect_equal(client$service(), "connect")
})
