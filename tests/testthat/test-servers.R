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

test_that("normalizes connect urls", {
  expected <- "https://myserver.com/__api__"

  expect_equal(ensureConnectServerUrl("https://myserver.com"), expected)
  expect_equal(ensureConnectServerUrl("https://myserver.com/"), expected)
  expect_equal(ensureConnectServerUrl("https://myserver.com/__api__"), expected)
  expect_equal(ensureConnectServerUrl("https://myserver.com/__api__/"), expected)
})
