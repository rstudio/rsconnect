test_that("basic HTTP methods work (httr2)", {
  skip_if_not_installed("webfakes")

  test_http_GET()
  test_http_POST_JSON()
  test_http_POST_empty()
  test_http_POST_file()
  test_http_headers()
})
