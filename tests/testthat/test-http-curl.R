test_that("basic HTTP methods work", {
  skip_if(Sys.which("curl") == "")
  skip_if_not_installed("webfakes")

  withr::local_options(rsconnect.http = "curl", lifecycle_verbosity = "quiet")

  test_http_GET()
  test_http_POST_JSON()
  test_http_POST_empty()
  test_http_POST_file()
  test_http_headers()
})
