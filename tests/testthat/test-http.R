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
  expect_snapshot({
    str(authHeaders(list(secret = "123"), url, "GET"))
  })
})
