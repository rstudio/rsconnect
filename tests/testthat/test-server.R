context("server")

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
