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

  body <- list(a = 1, b = 2, c = 3)
  resp <- POST_JSON(service, authInfo = NULL, path = "post", json = body)
  expect_equal(resp$status, 200)

  contents <- handleResponse(resp)
  expect_equal(contents$json, body)
}

test_http_POST_empty <- function() {
  service <- httpbin_service()

  resp <- POST(service, authInfo = NULL, path = "post")
  expect_equal(resp$status, 200)

  contents <- handleResponse(resp)
  expect_equal(contents$json, set_names(list()))
}

test_http_POST_file <- function(transport) {

  # Save the response the server will return
  saveRDS(file = input, object = list(
    status = 200L,
    headers = list(
      "Content-Type" = "text/plain"
    ),
    body = "POST successful"
  ))

  # Perform the request
  write(c("1", "2", "3"), datafile)
  POST(service = service,
       authInfo = NULL,
       path = "test",
       contentType = "text/plain",
       file = datafile,
       content = NULL)

  # Validate HTTP method
  request <- readRDS(output)
  expect_equal(request$REQUEST_METHOD, "POST")

  # Validate body contents
  expect(request$body == "1\n2\n3",
         failure_message =
           paste0("Unexpected request body '", request$body, "', with transport ", transport))
