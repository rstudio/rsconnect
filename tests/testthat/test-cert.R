test_that("certificates can be saved", {
  local_temp_config()

  # add a server with a sample certificate
  addTestServer(
    url = "https://localhost:4567/",
    name = "cert_test_a",
    certificate = test_path("certs/sample.crt")
  )

  # read it back
  info <- serverInfo("cert_test_a")

  # compare with the contents of the cert we read
  certLines <- paste(readLines(test_path("certs/sample.crt")), collapse = "\n")
  expect_equal(info$certificate, secret(certLines))
})

test_that("certificates can be added", {
  local_temp_config()

  # add a server without a certificate then add the certificate
  addTestServer(url = "https://localhost:4567/", name = "cert_test_b")
  addServerCertificate(
    name = "cert_test_b",
    certificate = test_path("certs/sample.crt"),
    quiet = TRUE
  )

  # read the server info
  info <- serverInfo("cert_test_b")

  # see if the cert we added is there
  certLines <- paste(readLines(test_path("certs/sample.crt")), collapse = "\n")
  expect_equal(info$certificate, secret(certLines))
})

test_that("certificates can't be attached to plain http servers", {
  local_temp_config()

  expect_error(addTestServer(
    url = "http://localhost:4567",
    name = "cert_test_c",
    certificate = test_path("certs/sample.crt")
  ))
  addTestServer(url = "http://localhost:4567", name = "cert_test_d")
  expect_error(addServerCertificate(
    name = "cert_test_d",
    certificate = test_path("certs/sample.crt")
  ))
})

test_that("system and server cert stores are concatenated", {
  local_temp_config()

  # use a dummy CA bundle
  withr::local_options(rsconnect.ca.bundle = test_path("certs/store.crt"))

  # create and then read the temporary certificate file
  concatenated <- createCertificateFile(readLines(test_path("certs/localhost.crt")))
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

test_that("multiple certificates can exist in the same file", {
  local_temp_config()

  addTestServer(
    url = "https://localhost:4567/",
    name = "cert_test_f",
    certificate = test_path("certs/two-cas.crt")
  )

  # read it back
  info <- serverInfo("cert_test_f")

  # compare with the contents of the cert we read
  certLines <- paste(readLines(test_path("certs/two-cas.crt")), collapse = "\n")
  expect_equal(info$certificate, secret(certLines))
})

test_that("certificates not used when making plain http connections", {
  local_temp_config()
  local_http_recorder()

  GET(
    list(
      protocol  = "http",
      host      = "localhost:4567",
      port      = "80",
      path      = "apps"
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
      protocol  = "https",
      host      = "localhost:4567",
      port      = "443",
      path      = "apps"
    ),
    authInfo = list(certificate = test_path("certs/localhost.crt")),
    "apps"
  )

  # we expect to get a cert file
  expect_true(file.exists(httpLastRequest$certificate))

  # clean up
  unlink(httpLastRequest$certificate)
})
