cache <- new_environment()

httpbin_service <- function() {
  app <- env_cache(
    cache,
    "test_app",
    webfakes::new_app_process(webfakes::httpbin_app())
  )

  parseHttpUrl(app$url())
}

# Generic tests of various http methods -----------------------------------

test_http_GET <- function() {
  service <- httpbin_service()

  # Perform the request
  resp <- GET(service, authInfo = NULL, path = "get")
  expect_equal(resp$status, 200)
  expect_equal(resp$contentType, "application/json")

  contents <- handleResponse(resp)
  expect_equal(contents$path, "/get")
}

test_http_POST_JSON <- function() {
  service <- httpbin_service()

  # Perform the request
  body <- list(a = 1, b = 2, c = 3)
  resp <- POST_JSON(service, authInfo = NULL, path = "anything", json = body)

  contents <- handleResponse(resp)
  expect_equal(contents$json, body)
}
