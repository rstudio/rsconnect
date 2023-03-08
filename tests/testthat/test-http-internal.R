test_that("basic HTTP methods work", {
  withr::local_options(rsconnect.http = "internal")

  test_http_GET()
  test_http_POST_JSON()
})
