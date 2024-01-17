test_that("non-libCurl methods are deprecated", {
  withr::local_options(rsconnect.http = "internal")
  expect_snapshot(. <- httpFunction())
})

# headers -----------------------------------------------------------------

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

  local_mocked_bindings(
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

test_that("can add user specific headers", {
  withr::local_options(rsconnect.http.headers = c(a = "1", b = "2"))

  service <- httpbin_service()
  json <- GET(service, list(), "get")
  expect_equal(json$headers$a, "1")
  expect_equal(json$headers$b, "2")
})

test_that("can add user specific cookies", {
  skip_on_cran()
  # uses live httpbin since webfakes doesn't support cookie endpoints
  withr::local_options(rsconnect.http.cookies = c("a=1", "b=2"))
  service <- parseHttpUrl("http://httpbin.org/")

  skip_on_http_failure(json <- GET(service, list(), "cookies"))
  expect_equal(json$cookies, list(a = "1", b = "2"))

  withr::local_options(rsconnect.http.cookies = c("c=3", "d=4"))
  skip_on_http_failure(POST(service, list(), "post"))
  skip_on_http_failure(json <- GET(service, list(), "cookies"))
  expect_equal(json$cookies, list(a = "1", b = "2", c = "3", d = "4"))
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
  }, transform = strip_port(service))
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

test_that("handles redirects", {
  service <- httpbin_service()
  out <- GET(service, list(), "absolute-redirect/3")
  expect_equal(out$url, paste0(buildHttpUrl(service), "get"))

  out <- GET(service, list(), "relative-redirect/3")
  expect_equal(out$url, paste0(buildHttpUrl(service), "get"))
})

# parse/build -------------------------------------------------------------

test_that("URL parsing works", {
  p <- parseHttpUrl("http://yahoo.com")
  expect_equal(p$protocol, "http")
  expect_equal(p$host, "yahoo.com")
  expect_equal(p$port, "")
  expect_equal(p$path, "") #TODO: bug? Should default to /?

  p <- parseHttpUrl("https://rstudio.com/about")
  expect_equal(p$protocol, "https")
  expect_equal(p$host, "rstudio.com")
  expect_equal(p$port, "")
  expect_equal(p$path, "/about")

  p <- parseHttpUrl("http://127.0.0.1:3939/stuff/here/?who-knows")
  expect_equal(p$protocol, "http")
  expect_equal(p$host, "127.0.0.1")
  expect_equal(p$port, "3939")
  expect_equal(p$path, "/stuff/here/?who-knows") #TODO: bug?
})

test_that("parse and build are symmetric", {
  round_trip <- function(x) {
    expect_equal(buildHttpUrl(parseHttpUrl(x)), x)
  }

  round_trip("http://google.com")
  round_trip("http://google.com:80")
  round_trip("https://google.com:80/a/b")
  round_trip("https://google.com:80/a/b/")
})

test_that("rcf2616 returns an ASCII date and undoes changes to the locale", {
  old <- Sys.getlocale("LC_TIME")
  defer(Sys.setlocale("LC_TIME", old))

  date <- rfc2616Date(time = as.POSIXct("2024-01-01 01:02:03", tz = "EST"))
  expect_equal(date, "Mon, 01 Jan 2024 06:02:03 GMT")
  expect_equal(Sys.getlocale("LC_TIME"), old)
})
