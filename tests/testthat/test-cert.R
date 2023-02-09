havingHttpRecorder <- function(expr) {
  withr::local_options(rsconnect.http = httpTestRecorder)
  eval(expr)
}

test_that("certificates can be saved", {
  havingFakeConfig(
    havingHttpRecorder({
      # add a server with a sample certificate
      addServer(url = "https://localhost:4567/",
                name = "cert_test_a",
                certificate = test_path("certs/sample.crt"),
                quiet = TRUE)

      # read it back
      info <- serverInfo("cert_test_a")

      # compare with the contents of the cert we read
      certLines <- paste(readLines(test_path("certs/sample.crt")), collapse = "\n")
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
                quiet = TRUE)

      # add the certificate
      addServerCertificate(name = "cert_test_b",
                           certificate = "certs/sample.crt",
                           quiet = TRUE)

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
      addServer(url = "http://localhost:4567", name = "cert_test_d", quiet = TRUE)
      expect_error(addServerCertificate(name = "cert_test_d",
                                        certificate = "certs/sample.crt"))
    })
  )
})

test_that("system and server cert stores are concatenated", {
  havingFakeConfig(
    havingHttpRecorder({
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
  )
})

test_that("invalid certificates cannot be added", {
  havingFakeConfig(
    havingHttpRecorder({
      expect_error(
        addServer(url = "https://localhost:4567/",
                  name = "cert_test_e",
                  certificate = "certs/invalid.crt",
                  quiet = TRUE))
    })
  )
})

test_that("multiple certificates can exist in the same file", {
  havingFakeConfig(
    havingHttpRecorder({
      addServer(url = "https://localhost:4567/",
                name = "cert_test_f",
                certificate = "certs/two-cas.crt",
                quiet = TRUE)

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
