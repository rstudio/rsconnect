test_that("basic HTTP methods work", {
  withr::local_options(rsconnect.http = "libcurl")

  test_http_GET()
  test_http_POST_JSON()
  test_http_POST_empty()
  test_http_POST_file()
  test_http_headers()
})

test_that("can trace JSON", {
  withr::local_options(rsconnect.http.trace.json = TRUE)
  service <- httpbin_service()

  expect_snapshot(transform = strip_port(service), {
    . <- GET(service, list(), "get")
    . <- POST_JSON(service, list(), "post", list(a = 1, b = 2))
  })
})

test_that("can get and set cookies", {
  skip_on_cran()
  # uses live httpbin since webfakes doesn't support cookie endpoints
  service <- parseHttpUrl("http://httpbin.org/")

  # Setting
  GET(service, list(), "cookies/set", query = "a=1")
  cookies <- getCookies(service$host)
  expect_equal(cookies$name, "a")
  expect_equal(cookies$value, "1")

  # Overwriting
  GET(service, list(), "cookies/set", query = "a=2&b=1")
  cookies <- getCookies(service$host)
  expect_equal(cookies$name, c("b", "a"))
  expect_equal(cookies$value, c("1", "2"))

  # Preserved across calls
  out <- GET(service, list(), "cookies")
  expect_equal(out$cookies, list(a = "2", b = "1"))
})
