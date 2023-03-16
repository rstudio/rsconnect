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

test_that("addServer() normalises url", {
  local_temp_config()

  addServer("connect.rstudioservices.com", name = "connect", quiet = TRUE)
  info <- serverInfo("connect")
  expect_equal(info$url, "https://connect.rstudioservices.com/__api__")
})

# cloud servers -----------------------------------------------------------

test_that("All hosted product names are identified as cloud", {
  expect_true(isCloudServer("shinyapps.io"))
  expect_true(isCloudServer("rstudio.cloud"))
  expect_true(isCloudServer("posit.cloud"))
  expect_false(isCloudServer("connect.internal"))
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

test_that("cloud server errors if not cloud server", {
  expect_snapshot(cloudServerInfo("foo"), error = TRUE)
})

# findServer --------------------------------------------------------------

test_that("findServer() errors if no servers", {
  local_temp_config()
  expect_snapshot(findServer(), error = TRUE)
})

test_that("findServer() picks server if only one present", {
  mockr::local_mock(
    servers = function(...) {
      data.frame(name = "myserver", stringsAsFactors = FALSE)
    }
  )
  expect_equal(findServer(), "myserver")
})

test_that("findServer() errors/prompts of multiple servers present", {
  mockr::local_mock(
    servers = function(...) {
      data.frame(name = c("myserver", "yourserver"), stringsAsFactors = FALSE)
    }
  )
  expect_snapshot(findServer(), error = TRUE)

  withr::local_options(
    rlang_interactive = TRUE,
    cli_prompt = "2"
  )
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
