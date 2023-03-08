test_that("simple http GET works", {
  withr::local_options(rsconnect.http = "rcurl")

  test_http_GET()
})
