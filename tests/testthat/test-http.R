test_that("authHeaders() picks correct method based on supplied fields", {
  url <- "https://example.com"

  expect_equal(
    authHeaders(list(), url, "GET"),
    list("X-Auth-Token" = "anonymous-access")
  )
  expect_equal(
    authHeaders(list(apiKey = "123"), url, "GET"),
    list(Authorization = "Key 123")
  )

  mockr::local_mock(
    rfc2616Date = function() "Thu, 09 Mar 2023 14:29:00 GMT"
  )

  # Dummy key created with
  # openssl::base64_encode(openssl::ed25519_keygen())
  key <- "MC4CAQAwBQYDK2VwBCIEIDztfEgkp5CX7Jz0NCyrToaRW1L2tfmrWxNDgYyjO9bQ"

  expect_snapshot({
    str(authHeaders(list(secret = "123"), url, "GET"))
    str(authHeaders(list(private_key = key), url, "GET"))
  })
})


# handleResponse ----------------------------------------------------------

test_that("includes body in error if available", {
  service <- parseHttpUrl("http://example.com/error")
  service$method <- "GET"

  resp_text <- list(
    req = service,
    status = 400,
    contentType = "plain/text",
    content = "Failed"
  )
  resp_json <- list(
    req = service,
    status = 400,
    contentType = "application/json",
    content = '{"error": "failed"}'
  )
  resp_html <- list(
    req = service,
    status = 400,
    contentType = "text/html",
    content = "<body>Failed</body>"
  )

  expect_snapshot(error = TRUE, {
    handleResponse(resp_text)
    handleResponse(resp_json)
    handleResponse(resp_html)
  })
})

test_that("but still gives got error if no body", {
  service <- parseHttpUrl("http://example.com/error")

  resp_text <- list(
    req = service,
    status = 400,
    contentType = "plain/text",
    content = ""
  )
  resp_json <- list(
    req = service,
    status = 400,
    contentType = "application/json",
    content = ""
  )
  resp_html <- list(
    req = service,
    status = 400,
    contentType = "text/html",
    content = ""
  )

  expect_snapshot(error = TRUE, {
    handleResponse(resp_text)
    handleResponse(resp_json)
    handleResponse(resp_html)
  })
})

test_that("errors contain method", {
  service <- httpbin_service()
  expect_snapshot(error = TRUE, {
    GET(service, list(), path = "status/404")
    POST(service, list(), path = "status/403")
  }, transform = function(x) gsub(service$port, "{port}", x))
})

test_that("http error includes status in error class", {
  service <- httpbin_service()
  expect_error(
    GET(service, list(), path = "status/404"),
    class = "rsconnect_http_404"
  )
  expect_error(
    GET(service, list(), path = "status/403"),
    class = "rsconnect_http_403"
  )
})
