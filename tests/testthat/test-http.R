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

test_that("throws useful errors when request fails", {
  service <- parseHttpUrl("http://example.com/error")
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
    content = '<body>Failed</body>'
  )

  expect_snapshot(error = TRUE, {
    handleResponse(resp_text)
    handleResponse(resp_json)
    handleResponse(resp_html)
  })
})

test_that("throws useful errors when request fails with empty body", {
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
