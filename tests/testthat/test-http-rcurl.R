test_that("basic HTTP methods work", {
  skip_if_not_installed("RCurl")
  withr::local_options(rsconnect.http = "rcurl", warnPartialMatchArgs = FALSE)

  test_http_GET()
  test_http_POST_JSON()
  test_http_POST_empty()
})
