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
