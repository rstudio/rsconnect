test_that("basic HTTP methods work", {
  skip_if_not_installed("webfakes")

  withr::local_options(rsconnect.http = "libcurl")

  test_http_GET()
  test_http_POST_JSON()
  test_http_POST_empty()
  test_http_POST_file()
  test_http_headers()
})

test_that("can trace JSON", {
  skip_if_not_installed("webfakes")

  withr::local_options(rsconnect.http.trace.json = TRUE)
  service <- httpbin_service()

  json_app <- webfakes::new_app()
  json_app$use(webfakes::mw_json())
  json_app$post("/", function(req, res) {
    res$set_status(200L)$send_json(req$json)
  })
  app <- webfakes::new_app_process(json_app)
  service <- parseHttpUrl(app$url())

  expect_snapshot({
    . <- POST_JSON(service, list(), "", list(a = 1, b = 2))
  })
})

test_that("can get and set cookies", {
  skip_on_cran()
  skip_if_not_installed("webfakes")

  # uses live httpbin since webfakes doesn't support cookie endpoints
  service <- parseHttpUrl("http://httpbin.org/")

  # Setting
  skip_on_http_failure(GET(service, list(), "cookies/set", query = "a=1"))
  cookies <- getCookies(service$host)
  expect_equal(cookies$name, "a")
  expect_equal(cookies$value, "1")

  # Overwriting
  skip_on_http_failure(GET(service, list(), "cookies/set", query = "a=2&b=1"))
  cookies <- getCookies(service$host)
  expect_equal(cookies$name, c("b", "a"))
  expect_equal(cookies$value, c("1", "2"))

  # Preserved across calls
  skip_on_http_failure(out <- GET(service, list(), "cookies"))
  expect_equal(out$cookies, list(a = "2", b = "1"))
})
