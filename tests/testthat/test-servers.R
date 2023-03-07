test_that("servers() can return 0 row data frame", {
  local_temp_config()

  out <- servers(local = TRUE)
  expect_equal(nrow(out), 0)
  expect_named(out, c("name", "url", "certificate"))
})

test_that("servers() redacts the certificate", {
  local_temp_config()

  # add a server with a sample certificate
  addServer(
    url = "https://localhost:4567/",
    name = "cert_test_a",
    certificate = test_path("certs/sample.crt"),
    quiet = TRUE
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

test_that("servers list", {
  allServers <- servers()$name
  predefinedServers <- c()
  for (server in allServers) {
    if (identical(server, shinyappsServerInfo()$name)) {
      predefinedServers <- append(predefinedServers, server)
    }
    if (identical(server, cloudServerInfo()$name)) {
      predefinedServers <- append(predefinedServers, server)
    }
  }
  expect_length(predefinedServers, 2)
  expect_true(is.element("posit.cloud", predefinedServers))
  expect_true(is.element("shinyapps.io", predefinedServers))
})

test_that("cloud server info matches name given if valid", {
  rstudioServer <- serverInfo("rstudio.cloud")
  expect_equal(rstudioServer$name, "rstudio.cloud")

  rstudioServer <- serverInfo("posit.cloud")
  expect_equal(rstudioServer$name, "posit.cloud")
})

test_that("cloud server info is modern if invalid", {
  server <- cloudServerInfo("someones.connect.server")
  expect_equal(server$name, "posit.cloud")
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
