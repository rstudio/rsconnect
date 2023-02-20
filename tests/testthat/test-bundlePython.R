test_that("getPython handles null python by checking RETICULATE_PYTHON", {
  skip_on_cran()

  withr::local_envvar(RETICULATE_PYTHON = "/usr/local/bin/python")
  expect_equal(getPython(NULL), "/usr/local/bin/python")
})

test_that("getPython handles null python and empty RETICULATE_PYTHON by checking RETICULATE_PYTHON_FALLBACK", {
  skip_on_cran()

  withr::local_envvar(
    RETICULATE_PYTHON = NA,
    RETICULATE_PYTHON_FALLBACK = "/usr/local/bin/python"
  )
  expect_equal(getPython(NULL), "/usr/local/bin/python")
})

test_that("getPython handles null python, empty RETICULATE_PYTHON, and empty RETICULATE_PYTHON_FALLBACK", {
  skip_on_cran()

  withr::local_envvar(
    RETICULATE_PYTHON = NA,
    RETICULATE_PYTHON_FALLBACK = NA
  )
  expect_equal(getPython(NULL), NULL)
})

test_that("getPython expands paths", {
  skip_on_cran()

  expect_equal(getPython("~/bin/python"), path.expand("~/bin/python"))
})

test_that("getPythonForTarget honors rsconnect.python.enabled = FALSE", {
  skip_on_cran()

  withr::local_options(rsconnect.python.enabled = FALSE)
  result <- getPythonForTarget("/usr/bin/python", list(server = "shinyapps.io"))
  expect_equal(result, NULL)
})

test_that("getPythonForTarget honors rsconnect.python.enabled = TRUE", {
  skip_on_cran()

  withr::local_options(rsconnect.python.enabled = TRUE)
  result <- getPythonForTarget("/usr/bin/python", list(server = "shinyapps.io"))
  expect_equal(result, "/usr/bin/python")
})

test_that("getPythonForTarget defaults to enabled for Connect", {
  skip_on_cran()

  result <- getPythonForTarget(
    "/usr/bin/python",
    list(server = "connect.example.com")
  )
  expect_equal(result, "/usr/bin/python")
})

test_that("getPythonForTarget defaults to disabled for shinyapps.io", {
  skip_on_cran()

  result <- getPythonForTarget(
    "/usr/bin/python",
    list(server = "shinyapps.io")
  )
  expect_equal(result, NULL)
})

test_that("getPythonForTarget defaults to enabled for rstudio.cloud", {
  skip_on_cran()

  result <- getPythonForTarget(
    "/usr/bin/python",
    list(server = "rstudio.cloud")
  )
  expect_equal(result, "/usr/bin/python")
})

test_that("Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()

  env <- inferPythonEnv(test_path("test-reticulate-rmds"), pythonPathOrSkip())
  expect_named(env, c("version", "package_manager"))
  expect_named(env$package_manager, c("name", "version", "package_file", "contents"))
})
