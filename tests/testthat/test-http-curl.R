test_that("simple http GET works", {
  withr::local_options(rsconnect.http = "curl")

  test_http_GET()
})
