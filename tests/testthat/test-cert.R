context("certificates")

havingHttpRecorder <- function(expr) {
  # Preserve incoming state.
  http <- getOption("rsconnect.http")

  on.exit({
    # Restore incoming state on exit.
    options(rsconnect.http = http)
  }, add = TRUE)

  # Record HTTP calls rather than actually performing them
  options(rsconnect.http = httpTestRecorder)

  eval(expr)
}

test_that("certificates can be saved", {
  havingFakeConfig(
    havingHttpRecorder({
      # add a server with a sample certificate
      addServer(url = "https://localhost:4567/",
                name = "cert_test_a",
                certificate = "certs/sample.crt",
                quiet = FALSE)

      # read it back
      info <- serverInfo("cert_test_a")

      # compare with the contents of the cert we read
      certLines <- paste(readLines("certs/sample.crt"), collapse = "\n")
      expect_equal(certLines, info$certificate)
    })
  )
})

test_that("certificates can be added", {
  havingFakeConfig(
    havingHttpRecorder({
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
  )
})

test_that("certificates can't be attached to plain http servers", {
  havingFakeConfig(
    havingHttpRecorder({
      expect_error(addServer(url = "http://localhost:4567",
                             name = "cert_test_c",
                             certificate = "certs/sample.crt"))
      addServer(url = "http://localhost:4567", name = "cert_test_d")
      expect_error(addServerCertificate(name = "cert_test_d",
                                        certificate = "certs/sample.crt"))
    })
  )
})

test_that("system and server cert stores are concatenated", {
  havingFakeConfig(
    havingHttpRecorder({
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
  )
})

test_that("invalid certificates cannot be added", {
  havingFakeConfig(
    havingHttpRecorder({
      expect_error(
        addServer(url = "https://localhost:4567/",
                  name = "cert_test_e",
                  certificate = "certs/invalid.crt",
                  quiet = FALSE))
    })
  )
})

test_that("multiple certificates can exist in the same file", {
  havingFakeConfig(
    havingHttpRecorder({
      addServer(url = "https://localhost:4567/",
                name = "cert_test_f",
                certificate = "certs/two-cas.crt",
                quiet = FALSE)

      # read it back
      info <- serverInfo("cert_test_f")

      # compare with the contents of the cert we read
      certLines <- paste(readLines("certs/two-cas.crt"), collapse = "\n")
      expect_equal(certLines, info$certificate)
    })
  )
})

test_that("certificates not used when making plain http connections", {
  havingFakeConfig(
    havingHttpRecorder({
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
  )
})

test_that("certificates used when making https connections", {
  havingFakeConfig(
    havingHttpRecorder({
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
  )
})
