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
