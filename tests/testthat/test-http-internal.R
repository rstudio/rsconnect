test_that("basic HTTP methods work", {
  withr::local_options(rsconnect.http = "internal", lifecycle_verbosity = "quiet")

  test_http_GET()
  test_http_POST_JSON()
  test_http_POST_empty()
  test_http_POST_file()
  test_http_headers()
})
