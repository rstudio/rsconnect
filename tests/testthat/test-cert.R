context("certificates")

cert_test_that <- function(test, expr) {
  config_dir <- tempdir()

  # preserve old values
  home <- Sys.getenv("HOME")
  http <- getOption("rsconnect.http")

  on.exit({
    # clean up temp folder
    unlink(config_dir, recursive = TRUE)

    # restore HOME
    Sys.setenv(HOME = home, add = TRUE)

    # clean up options
    options(rsconnect.http = http)
  }, add = TRUE)

  # temporarily change home to a temp folder so we don't litter the actual
  # config folder with test output
  Sys.setenv(HOME = config_dir)

  # record HTTP calls rather than actually performing them
  options(rsconnect.http = httpTestRecorder)

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

cert_test_that("multiple certificates can exist in the same file", {
  addServer(url = "https://localhost:4567/",
            name = "cert_test_f",
            cert = "certs/two-cas.crt",
            quiet = FALSE)

  # read it back
  info <- serverInfo("cert_test_f")

  # compare with the contents of the cert we read
  certLines <- paste(readLines("certs/two-cas.crt"), collapse = "\n")
  expect_equal(certLines, info$certificate)
})

cert_test_that("certificates not used when making plain http connections", {
  GET(list(
        protocol  = "http",
        host      = "localhost:4567",
        port      = "80",
        path      = "apps"
      ),
      authInfo = list(
        certificate = "certs/localhost.crt"
      ),
      "apps")
  expect_equal(httpLastRequest$certificate, NULL)
})

cert_test_that("certificates used when making https connections", {
  GET(list(
        protocol  = "https",
        host      = "localhost:4567",
        port      = "443",
        path      = "apps"
      ),
      authInfo = list(
        certificate = "certs/localhost.crt"
      ),
      "apps")

  # we expect to get a cert file
  expect_true(file.exists(httpLastRequest$certificate))

  # clean up
  unlink(httpLastRequest$certificate)
})
