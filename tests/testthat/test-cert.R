test_that("system and server cert stores are concatenated", {
  local_temp_config()

  serverCertificateFile <- test_path("certs/localhost.pem")
  serverCertificate <- paste(
    c(
      # this in-memory certificate has duplication, which
      # is removed in the concatenated result.
      readLines(con = serverCertificateFile, warn = FALSE),
      readLines(con = serverCertificateFile, warn = FALSE)
    ),
    collapse = "\n"
  )

  caCertificateFile <- test_path("certs/example.com.pem")

  withr::local_options(rsconnect.ca.bundle = caCertificateFile)

  # create and then read the temporary certificate file
  concatenated <- createCertificateFile(serverCertificate)
  withr::defer(unlink(concatenated))

  # the result is the concatenation (ca first) without duplicates.
  expect_equal(
    openssl::read_cert_bundle(concatenated),
    openssl::read_cert_bundle(
      paste0(
        sapply(
          c(
            openssl::read_cert_bundle(caCertificateFile),
            openssl::read_cert_bundle(serverCertificateFile)
          ),
          openssl::write_pem
        ),
        collapse = ""
      )
    )
  )
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
  expect_null(requestCertificate("http", test_path("certs/localhost.pem")))
})

test_that("certificates used when making https connections", {
  local_temp_config()
  cert <- requestCertificate("https", test_path("certs/localhost.pem"))
  # we expect to get a cert file
  expect_true(file.exists(cert))

  # clean up
  unlink(cert)
})
