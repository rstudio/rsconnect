test_that("getPython looks in argument, RETICULATE_PYTHON, then RETICULATE_PYTHON_FALLBACK", {
  skip_on_cran()

  withr::local_envvar(
    RETICULATE_PYTHON = "~/python",
    RETICULATE_PYTHON_FALLBACK = "~/fallback"
  )
  expect_equal(getPython("~/supplied"), path.expand("~/supplied"))
  expect_equal(getPython(NULL), path.expand("~/python"))

  withr::local_envvar(
    RETICULATE_PYTHON = NA,
    RETICULATE_PYTHON_FALLBACK = "~/fallback"
  )
  expect_equal(getPython(NULL), path.expand("~/fallback"))

  withr::local_envvar(
    RETICULATE_PYTHON = NA,
    RETICULATE_PYTHON_FALLBACK = NA
  )
  expect_equal(getPython(NULL), NULL)
})

test_that("rsconnect.python.enabled overrides getPythonForTarget() default", {
  skip_on_cran()

  expect_equal(getPythonForTarget("p", list(server = "shinyapps.io")), NULL)
  expect_equal(getPythonForTarget("p", list(server = "example.com")), "p")

  withr::local_options(rsconnect.python.enabled = FALSE)
  expect_equal(getPythonForTarget("p", list(server = "shinyapps.io")), NULL)
  expect_equal(getPythonForTarget("p", list(server = "example.com")), NULL)

  withr::local_options(rsconnect.python.enabled = TRUE)
  expect_equal(getPythonForTarget("p", list(server = "shinyapps.io")), "p")
  expect_equal(getPythonForTarget("p", list(server = "example.com")), "p")
})

test_that("Rmd with reticulate as a dependency includes python in the manifest", {
  skip_on_cran()

  env <- inferPythonEnv(test_path("test-reticulate-rmds"), pythonPathOrSkip())
  expect_named(env, c("version", "package_manager"))
  expect_named(env$package_manager, c("name", "version", "package_file", "contents"))
})
