test_that("validateConnectUrl() returns expected return for some known endpoints", {

  expect_false(validateConnectUrl("http://posit.cloud")$valid)
  expect_false(validateConnectUrl("http://shinyapps.io")$valid)
  expect_true(validateConnectUrl("https://connect.rstudioservices.com/")$valid)
  expect_true(validateConnectUrl("https://colorado.posit.co/rsc")$valid)
})

test_that("validateConnectUrl() normalises urls", {
  api_url <- "https://connect.rstudioservices.com/__api__"
  expect_equal(validateConnectUrl("connect.rstudioservices.com")$url, api_url)
  expect_equal(validateConnectUrl("connect.rstudioservices.com")$url, api_url)
  expect_equal(validateConnectUrl("https://connect.rstudioservices.com/")$url, api_url)
})

test_that("getAppById() fails where expected", {
  local_temp_config()
  addTestServer()
  addTestAccount("susan")

  expect_snapshot(error = TRUE, {
    getAppById("123", "susan", "unknown", "unknown.com")
    getAppById("123", "robert", "unknown", "http://example.com")
  })
})
