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
            name = "cert_test",
            cert = "cert/sample.crt",
            quiet = FALSE)

  # read it back
  info <- serverInfo("cert_test")

  # compare with the contents of the cert we read
  certLines <- readLines("cert/sample.crt")
  expect_equal(certLines, info$certificate)
})
