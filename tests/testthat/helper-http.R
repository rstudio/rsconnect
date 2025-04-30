cache <- new_environment()

httpbin_service <- function() {
  app <- env_cache(
    cache,
    "test_app",
    webfakes::new_app_process(webfakes::httpbin_app())
  )

  parseHttpUrl(app$url())
}

strip_port <- function(service) {
  function(x) gsub(service$port, "{port}", x)
}

local_cookie_store <- function(env = caller_env()) {
  nms <- env_names(.cookieStore)
  zaps <- rep_named(nms, list(zap()))

  old <- env_bind(.cookieStore, !!!zaps)
  withr::defer(env_bind(.cookieStore, !!!old), envir = env)
}

skip_on_http_failure <- function(code) {
  tryCatch(
    code,
    rsconnect_http = function(cnd) {
      testthat::skip("http request failed")
    }
  )
}

# Service that 404s for server settings.
service_settings_404 <- function() {
  app <- env_cache(
    cache,
    "service_settings_404",
    {
      json_app <- webfakes::new_app()
      json_app$use(webfakes::mw_json())
      json_app$get("/__api__/server_settings", function(req, res) {
        res$set_status(404L)$send_json(list(
          error = jsonlite::unbox("not found")
        ))
      })
      app <- webfakes::new_app_process(json_app)
    }
  )
  parseHttpUrl(app$url())
}

# Service that 200s for server settings.
service_settings_200 <- function() {
  app <- env_cache(
    cache,
    "service_settings_200",
    {
      json_app <- webfakes::new_app()
      json_app$use(webfakes::mw_json())
      json_app$get("/__api__/server_settings", function(req, res) {
        res$set_status(200L)$send_json(list(data = jsonlite::unbox("ok")))
      })
      app <- webfakes::new_app_process(json_app)
    }
  )
  parseHttpUrl(app$url())
}

# Service that redirects for server settings.
service_redirect <- function(target) {
  app <- env_cache(
    cache,
    "service_redirect",
    {
      json_app <- webfakes::new_app()
      json_app$use(webfakes::mw_json())
      json_app$get("/__api__/server_settings", function(req, res) {
        res$redirect(target)
      })
      app <- webfakes::new_app_process(json_app)
    }
  )
  parseHttpUrl(app$url())
}


# Generic tests of various http methods -----------------------------------

test_http_GET <- function() {
  service <- httpbin_service()

  # Perform the request
  resp <- GET(service, authInfo = NULL, path = "get")
  expect_equal(attr(resp, "httpContentType"), "application/json")
  expect_equal(resp$path, "/get")
}

test_http_POST_JSON <- function() {
  service <- httpbin_service()

  body <- list(a = 1, b = 2, c = 3)
  resp <- POST_JSON(service, authInfo = NULL, path = "post", json = body)
  expect_equal(resp$json, body)
}

test_http_POST_empty <- function() {
  service <- httpbin_service()

  resp <- POST(service, authInfo = NULL, path = "post")
  expect_equal(resp$json, set_names(list()))
}

test_http_POST_file <- function() {
  service <- httpbin_service()

  path <- withr::local_tempfile()
  con <- file(path, "wb")
  writeLines(c("1", "2", "3"), con = con)
  close(con)

  resp <- POST(
    service,
    authInfo = NULL,
    path = "post",
    contentType = "text/plain",
    file = path
  )
  expect_equal(resp$data, "1\n2\n3\n")
}

test_http_headers <- function() {
  service <- httpbin_service()

  resp <- GET(service, authInfo = list(apiKey = "abc123"), path = "get")
  expect_equal(resp$headers$Authorization, "Key abc123")

  resp <- POST(service, authInfo = list(apiKey = "abc123"), path = "post")
  expect_equal(resp$headers$Authorization, "Key abc123")
}
