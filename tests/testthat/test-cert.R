context("certificates")

cert_test_that <- function(test, expr) {
  # temporarily change home to a temp folder so we don't litter the actual
  # config folder with test output
  config_dir <- tempdir()
  home <- Sys.getenv("HOME")
  on.exit({
    # clean up temp folder and restore HOME when done
    unlink(config_dir, recursive = TRUE)
    Sys.setenv(HOME = home, add = TRUE)
  })
  Sys.setenv(HOME = config_dir)

  test_that(test, expr)
}

cert_test_that("certificates can be saved", {

  # add a server with a sample certificate
  addServer(url = "https://localhost:4567/",
            name = "cert_test_a",
            cert = "certs/sample.crt",
            quiet = FALSE)

  # read it back
  info <- serverInfo("cert_test_a")

  # compare with the contents of the cert we read
  certLines <- paste(readLines("certs/sample.crt"), collapse = "\n")
  expect_equal(certLines, info$certificate)
})

cert_test_that("certificates can be added", {
  # add a server without a certificate
  addServer(url = "https://localhost:4567/",
            name = "cert_test_b",
            quiet = FALSE)

  # add the certificate
  addServerCertificate(name = "cert_test_b",
                       certificate = "certs/sample.crt")

  # read the server info
  info <- serverInfo("cert_test_b")

  # see if the cert we added is there
  certLines <- paste(readLines("certs/sample.crt"), collapse = "\n")
  expect_equal(certLines, info$certificate)
})

cert_test_that("certificates can't be attached to plain http servers", {
  expect_error(addServer(url = "http://localhost:4567",
                         name = "cert_test_c",
                         certificate = "certs/sample.crt"))
  addServer(url = "http://localhost:4567", name = "cert_test_d")
  expect_error(addServerCertificate(name = "cert_test_d",
                                    certificate = "certs/sample.crt"))
})

cert_test_that("system and server cert stores are concatenated", {
  # use a dummy CA bundle
  oldCaOpt <- getOption("rsconnect.ca.bundle")
  options(rsconnect.ca.bundle = file.path(getwd(), "certs", "store.crt"))
  on.exit(expr = {
    options(rsconnect.ca.bundle = oldCaOpt)
  }, add = TRUE)

  # create and then read the temporary certificate file
  concatenated <- createCertificateFile(readLines("certs/localhost.crt"))
  on.exit(expr = {
    unlink(concatenated)
  }, add = TRUE)
  store <- paste(readLines(concatenated), collapse = "\n")

  # make sure that the localhost and system stores both exist in the
  # concatenated store
  expect_true(grepl("localhost", store, fixed = TRUE))
  expect_true(grepl("system", store, fixed = TRUE))
})

cert_test_that("invalid certificates cannot be added", {
  expect_error(
    addServer(url = "https://localhost:4567/",
              name = "cert_test_e",
              cert = "certs/invalid.crt",
              quiet = FALSE))
})
