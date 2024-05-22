test_that("servers() can return 0 row data frame", {
  local_temp_config()

  out <- servers(local = TRUE)
  expect_equal(nrow(out), 0)
  expect_named(out, c("name", "url", "certificate"))
})

test_that("servers() redacts the certificate", {
  local_temp_config()

  # add a server with a sample certificate
  addTestServer(
    url = "https://localhost:4567/",
    name = "cert_test_a",
    certificate = test_path("certs/sample.crt")
  )

  expect_snapshot(servers())
})

test_that("serverInfo() redacts the certificate", {
  expect_snapshot({
    str(serverInfo("posit.cloud"))
    str(serverInfo("shinyapps.io"))
  })
})

test_that("serverInfo() errors if server not present", {
  local_temp_config()

  expect_snapshot(serverInfo("foo"), error = TRUE)
})

test_that("normalizes connect urls", {
  expected <- "https://myserver.com/__api__"

  expect_equal(ensureConnectServerUrl("https://myserver.com"), expected)
  expect_equal(ensureConnectServerUrl("https://myserver.com/"), expected)
  expect_equal(ensureConnectServerUrl("https://myserver.com/__api__"), expected)
  expect_equal(ensureConnectServerUrl("https://myserver.com/__api__/"), expected)
})


# addServer ---------------------------------------------------------------

test_that("addServer() name defaults to hostname & port of url", {
  expect_equal(serverName("https://example.com/abc"), "example.com")
  expect_equal(serverName("https://example.com:8787/abc"), "example.com:8787")
})

test_that("addServer() normalises url", {
  skip_on_cran()
  local_temp_config()

  addServer("connect.posit.it", name = "connect", quiet = TRUE)
  info <- serverInfo("connect")
  expect_equal(info$url, "https://connect.posit.it/__api__")
})

test_that("addServer() errors if url not a connect server", {
  skip_if_not_installed("webfakes")

  local_temp_config()

  service <- httpbin_service()
  url <- buildHttpUrl(service)
  expect_snapshot(addServer(url), error = TRUE)
})

test_that("addServer() and addServerCertificate() inform about their actions", {
  local_temp_config()

  cert <- test_path("certs/sample.crt")
  expect_snapshot({
    addServer("https://example.com", validate = FALSE)
    addServerCertificate("example.com", certificate = cert)
  })
})

test_that("can save certificates", {
  local_temp_config()

  addTestServer("test", certificate = test_path("certs/sample.crt"))

  info <- serverInfo("test")
  certLines <- paste(readLines(test_path("certs/sample.crt")), collapse = "\n")
  expect_equal(info$certificate, secret(certLines))
})

test_that("can add certificates after creation", {
  local_temp_config()

  addTestServer("test")
  addServerCertificate("test",
    certificate = test_path("certs/sample.crt"),
    quiet = TRUE
  )

  info <- serverInfo("test")
  certLines <- paste(readLines(test_path("certs/sample.crt")), collapse = "\n")
  expect_equal(info$certificate, secret(certLines))
})

test_that("can store multiple certificates can exist in one dcf", {
  local_temp_config()

  addTestServer("test", certificate = test_path("certs/two-cas.crt"))

  info <- serverInfo("test")
  certLines <- paste(readLines(test_path("certs/two-cas.crt")), collapse = "\n")
  expect_equal(info$certificate, secret(certLines))
})

test_that("certificates can't be attached to plain http servers", {
  local_temp_config()

  addTestServer("test", "http://example.com")
  cert <- test_path("certs/sample.crt")
  expect_snapshot(addServerCertificate("test", cert), error = TRUE)
})

# cloud servers -----------------------------------------------------------

test_that("All hosted product names are identified as cloud", {
  expect_true(isCloudServer("shinyapps.io"))
  expect_true(isCloudServer("rstudio.cloud"))
  expect_true(isCloudServer("posit.cloud"))
  expect_false(isCloudServer("connect.internal"))
})

test_that("All hosted product names are identified as cloud", {
  checkCloudServer("shinyapps.io")
  checkCloudServer("rstudio.cloud")
  checkCloudServer("posit.cloud")
  expect_error(checkCloudServer("connect.internal"))
})

test_that("only shinyapps.io is identified as shinyapps.io", {
  expect_true(isShinyappsServer("shinyapps.io"))
  expect_false(isShinyappsServer("rstudio.cloud"))
  expect_false(isShinyappsServer("posit.cloud"))
  expect_false(isShinyappsServer("connect.internal"))
})

test_that("only shinyapps.io is identified as shinyapps.io", {
  checkShinyappsServer("shinyapps.io")
  expect_error(checkShinyappsServer("rstudio.cloud"))
  expect_error(checkShinyappsServer("posit.cloud"))
  expect_error(checkShinyappsServer("connect.internal"))
})

test_that("predefined servers includes cloud and shinyapps", {
  local_temp_config()

  out <- servers()
  expect_equal(nrow(out), 2)
  expect_named(out, c("name", "url", "certificate"))
  expect_setequal(out$name, c("posit.cloud", "shinyapps.io"))
})

test_that("predefined servers includes rstudio.cloud if needed", {
  local_temp_config()
  addTestAccount("john", "rstudio.cloud")
  expect_true("rstudio.cloud" %in% servers()$name)
})

test_that("cloud server info matches name given if valid", {
  local_temp_config()
  addTestAccount("john", "rstudio.cloud")

  rstudioServer <- serverInfo("rstudio.cloud")
  expect_equal(rstudioServer$name, "rstudio.cloud")
})

# findServer --------------------------------------------------------------

test_that("findServer() errors if no servers", {
  local_temp_config()
  expect_snapshot(findServer(), error = TRUE)
})

test_that("findServer() picks server if only one present", {
  local_mocked_bindings(servers = function(...) {
    data.frame(name = "myserver", stringsAsFactors = FALSE)
  })
  expect_equal(findServer(), "myserver")
})

test_that("findServer() errors/prompts of multiple servers present", {
  local_mocked_bindings(servers = function(...) {
    data.frame(name = c("myserver", "yourserver"), stringsAsFactors = FALSE)
  })
  expect_snapshot(findServer(), error = TRUE)

  simulate_user_input(2)
  expect_snapshot(out <- findServer())
  expect_equal(out, "yourserver")
})

test_that("findServer checks server name", {
  local_temp_config()

  expect_snapshot(error = TRUE, {
    findServer(1)
    findServer("foo")
  })
})
