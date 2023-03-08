test_that("simple http GET works", {
  withr::local_options(rsconnect.http = "libcurl")

  test_http_GET()
})
