test_that("system and server cert stores are concatenated", {
  local_temp_config()

  # use a dummy CA bundle
  withr::local_options(rsconnect.ca.bundle = test_path("certs/store.crt"))

  # create and then read the temporary certificate file
  concatenated <- createCertificateFile(readLines(test_path(
    "certs/localhost.crt"
  )))
  withr::defer(unlink(concatenated))
  store <- paste(readLines(concatenated), collapse = "\n")

  # make sure that the localhost and system stores both exist in the
  # concatenated store
  expect_true(grepl("localhost", store, fixed = TRUE))
  expect_true(grepl("system", store, fixed = TRUE))
})

test_that("invalid certificates cannot be added", {
  local_temp_config()

  expect_error(addTestServer(
    url = "https://localhost:4567/",
    name = "cert_test_e",
    certificate = test_path("certs/invalid.crt")
  ))
})

test_that("certificates not used when making plain http connections", {
  local_temp_config()
  local_http_recorder()

  GET(
    list(
      protocol = "http",
      host = "localhost:4567",
      port = "80",
      path = "apps"
    ),
    authInfo = list(certificate = test_path("certs/localhost.crt")),
    "apps"
  )
  expect_equal(httpLastRequest$certificate, NULL)
})

test_that("certificates used when making https connections", {
  local_temp_config()
  local_http_recorder()

  GET(
    list(
      protocol = "https",
      host = "localhost:4567",
      port = "443",
      path = "apps"
    ),
    authInfo = list(certificate = test_path("certs/localhost.crt")),
    "apps"
  )

  # we expect to get a cert file
  expect_true(file.exists(httpLastRequest$certificate))

  # clean up
  unlink(httpLastRequest$certificate)
})
