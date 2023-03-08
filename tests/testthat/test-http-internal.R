test_that("simple http GET works", {
  withr::local_options(rsconnect.http = "internal")

  test_http_GET()
})
