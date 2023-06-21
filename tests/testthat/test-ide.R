test_that("validateServerUrl() returns expected", {
  expect_false(validateServerUrl("https://posit.cloud")$valid)
  expect_false(validateServerUrl("https://shinyapps.io")$valid)
  expect_true(validateServerUrl("https://connect.rstudioservices.com/")$valid)
  expect_true(validateServerUrl("https://colorado.posit.co/rsc")$valid)
})

test_that("validateServerUrl() normalises urls", {
  expect_true(validateServerUrl("connect.rstudioservices.com/")$valid)
  expect_true(validateServerUrl("colorado.posit.co/rsc")$valid)
})

test_that("validateConnectUrl() returns expected return for some known endpoints", {
  expect_false(validateConnectUrl("https://posit.cloud")$valid)
  expect_false(validateConnectUrl("https://shinyapps.io")$valid)
  expect_true(validateConnectUrl("https://connect.rstudioservices.com/")$valid)
  expect_true(validateConnectUrl("https://colorado.posit.co/rsc")$valid)
})

test_that("validateConnectUrl() normalises urls", {
  api_url <- "https://connect.rstudioservices.com/__api__"
  expect_equal(validateConnectUrl("connect.rstudioservices.com")$url, api_url)
  expect_equal(validateConnectUrl("connect.rstudioservices.com")$url, api_url)
  expect_equal(validateConnectUrl("https://connect.rstudioservices.com/")$url, api_url)
})

test_that("validateConnectUrl() follows redirects", {
  api_url <- "https://connect.rstudioservices.com:443/__api__"
  expect_equal(validateConnectUrl("http://connect.rstudioservices.com")$url, api_url)
})

test_that("getAppById() fails where expected", {
  local_temp_config()
  addTestServer()
  addTestAccount("susan")

  expect_snapshot(error = TRUE, {
    getAppById("123", "susan", "unknown", "unknown.com")
    getAppById("123", "robert", "unknown", "https://example.com")
  })
})
