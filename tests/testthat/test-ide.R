test_that("validateServerUrl() returns expected", {
  skip_on_cran()

  expect_false(validateServerUrl("https://posit.cloud")$valid)
  expect_false(validateServerUrl("https://shinyapps.io")$valid)
  expect_true(validateServerUrl("https://connect.posit.it/")$valid)
  expect_true(validateServerUrl("https://colorado.posit.co/rsc")$valid)
})

test_that("validateServerUrl() normalises urls", {
  skip_on_cran()

  expect_true(validateServerUrl("connect.posit.it/")$valid)
  expect_true(validateServerUrl("colorado.posit.co/rsc")$valid)
})

test_that("validateConnectUrl() returns expected return for some known endpoints", {
  skip_on_cran()

  expect_false(validateConnectUrl("https://posit.cloud")$valid)
  expect_false(validateConnectUrl("https://shinyapps.io")$valid)
  expect_true(validateConnectUrl("https://connect.posit.it/")$valid)
  expect_true(validateConnectUrl("https://colorado.posit.co/rsc")$valid)
})

test_that("validateConnectUrl() normalises urls", {
  skip_on_cran()

  api_url <- "https://connect.posit.it/__api__"
  expect_equal(validateConnectUrl("connect.posit.it")$url, api_url)
  expect_equal(validateConnectUrl("connect.posit.it")$url, api_url)
  expect_equal(validateConnectUrl("https://connect.posit.it/")$url, api_url)
})

test_that("validateConnectUrl() follows redirects", {
  skip_on_cran()

  api_url <- "https://connect.posit.it:443/__api__"
  expect_equal(validateConnectUrl("http://connect.posit.it")$url, api_url)
})

test_that("getAppById() fails where expected", {
  local_temp_config()
  addTestServer()
  addTestAccount("susan")

  expect_snapshot(error = TRUE, {
    getAppById("123", "susan", "unknown", "unknown.com")
    getAppById("123", "robert", "unknown", "https://example.com")
  })
})

current_user_service <- function() {
  app <- env_cache(
    cache,
    "current_user_app",
    {
      json_app <- webfakes::new_app()
      json_app$use(webfakes::mw_json())
      json_app$get("/users/current", function(req, res) {
        res$set_status(200L)$send_json(list(username = jsonlite::unbox("susan")))
      })
      app <- webfakes::new_app_process(json_app)
    }
  )
  parseHttpUrl(app$url())
}

test_that("getUserFromRawToken having a single matching server", {
  skip_if_not_installed("webfakes")

  local_temp_config()

  service <- current_user_service()
  url <- buildHttpUrl(service)

  addTestServer("test", url = url)

  token <- generateToken()
  claimUrl <- url

  user <- getUserFromRawToken(claimUrl, token$token, token$private_key)
  expect_equal(user$username, "susan")
})

test_that("getUserFromRawToken having multiple matching servers", {
  skip_if_not_installed("webfakes")

  local_temp_config()

  service <- current_user_service()
  url <- buildHttpUrl(service)

  addTestServer("test", url = url)
  addTestServer("test2", url = url)

  token <- generateToken()
  claimUrl <- url

  user <- getUserFromRawToken(claimUrl, token$token, token$private_key)
  expect_equal(user$username, "susan")
})
