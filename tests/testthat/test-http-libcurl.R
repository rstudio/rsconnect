test_that("basic HTTP methods work", {
  withr::local_options(rsconnect.http = "libcurl")

  test_http_GET()
  test_http_POST_JSON()
  test_http_POST_empty()
  test_http_POST_file()
  test_http_headers()
})

test_that("can stream PUT content from disk", {
  service <- httpbin_service()

  path <- withr::local_tempfile()
  con <- file(path, "wb")
  writeLines(c("1", "2", "3"), con = con)
  close(con)

  resp <- PUT(
    service,
    authInfo = NULL,
    path = "put",
    contentType = "text/plain",
    file = path
  )
  expect_equal(attr(resp, "httpResponse")$status, 200)
  expect_equal(resp$data, "1\n2\n3\n")
})
