test_that("can snapshot deps with renv", {
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  app_dir <- local_temp_app(list(foo.R = "library(foreign); library(MASS)"))

  expect_snapshot(pkgs <- bundlePackages(app_dir))

  # renv is not a dependency
  expect_named(pkgs, c("foreign", "MASS"), ignore.order = TRUE)
  expect_named(pkgs$foreign, c("Source", "Repository", "description"))
  expect_named(pkgs$MASS, c("Source", "Repository", "description"))

  # No renv lockfile left behind
  expect_equal(list.files(app_dir), "foo.R")
})

test_that("can snapshot deps with packrat (option)", {
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  withr::local_options(rsconnect.packrat = TRUE)

  app_dir <- local_temp_app(list(foo.R = "library(foreign); library(MASS)"))

  expect_snapshot(pkgs <- bundlePackages(app_dir))

  expect_named(pkgs, c("foreign", "MASS"), ignore.order = TRUE)
  expect_named(pkgs$foreign, c("Source", "Repository", "description"))
  expect_named(pkgs$MASS, c("Source", "Repository", "description"))

  # No packrat lockfile left behind
  expect_equal(list.files(app_dir), "foo.R")
})

test_that("can snapshot deps with packrat (env var)", {
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  withr::local_envvar(RSCONNECT_PACKRAT = "TRUE")

  app_dir <- local_temp_app(list(foo.R = "library(foreign); library(MASS)"))

  expect_snapshot(pkgs <- bundlePackages(app_dir))

  expect_named(pkgs, c("foreign", "MASS"), ignore.order = TRUE)
  expect_named(pkgs$foreign, c("Source", "Repository", "description"))
  expect_named(pkgs$MASS, c("Source", "Repository", "description"))

  # No packrat lockfile left behind
  expect_equal(list.files(app_dir), "foo.R")
})

test_that("can capture deps from renv lockfile", {
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  withr::local_options(renv.verbose = FALSE)

  app_dir <- local_temp_app(list(foo.R = "library(foreign); library(MASS)"))
  renv::snapshot(app_dir, prompt = FALSE)

  expect_snapshot(pkgs <- bundlePackages(app_dir))

  # renv is included by the renv.lock
  expect_named(pkgs, c("foreign", "MASS", "renv"), ignore.order = TRUE)
  expect_named(pkgs$foreign, c("Source", "Repository", "description"))
  expect_named(pkgs$MASS, c("Source", "Repository", "description"))

  # No renv directory left behind, but the renv.lock is preserved.
  expect_equal(list.files(app_dir), c("foo.R", "renv.lock"))
})

test_that("can capture deps with packrat even when renv lockfile present", {
  skip_if_not_installed("foreign")
  skip_if_not_installed("MASS")

  withr::local_envvar(RSCONNECT_PACKRAT = "TRUE")
  withr::local_options(renv.verbose = FALSE)

  app_dir <- local_temp_app(list(foo.R = "library(foreign); library(MASS)"))
  renv::snapshot(app_dir, prompt = FALSE)

  expect_snapshot(pkgs <- bundlePackages(app_dir))

  # renv is not a dependency (not using renv.lock)
  expect_named(pkgs, c("foreign", "MASS"), ignore.order = TRUE)
  expect_named(pkgs$foreign, c("Source", "Repository", "description"))
  expect_named(pkgs$MASS, c("Source", "Repository", "description"))

  # The (incoming) renv.lock is discarded, mirroring rsconnect pre 1.0.0.
  expect_equal(list.files(app_dir), "foo.R")
})

# -------------------------------------------------------------------------

test_that("error if can't find source", {
  local_mocked_bindings(snapshotRenvDependencies = function(...) {
    data.frame(
      Package = "shiny",
      Source = NA,
      Repository = NA,
      stringsAsFactors = FALSE
    )
  })

  app_dir <- withr::local_tempdir()
  writeLines(con = file.path(app_dir, "index.Rmd"), c(
    "```{r}",
    "library(shiny)",
    "```"
  ))

  expect_snapshot(
    . <- bundlePackages(app_dir),
    error = TRUE
  )
})

test_that("usePackrat respects the environment variable", {
  expect_false(usePackrat())

  withr::local_envvar(RSCONNECT_PACKRAT = "TRUE")
  expect_true(usePackrat())

  withr::local_envvar(RSCONNECT_PACKRAT = "FALSE")
  expect_false(usePackrat())
})

test_that("usePackrat respects the option", {
  expect_false(usePackrat())

  withr::local_options(rsconnect.packrat = TRUE)
  expect_true(usePackrat())

  withr::local_options(rsconnect.packrat = FALSE)
  expect_false(usePackrat())
})

test_that("usePackrat prefers the environment variable over the option", {
  expect_false(usePackrat())

  withr::local_envvar(RSCONNECT_PACKRAT = "TRUE")
  withr::local_options(rsconnect.packrat = FALSE)
  expect_true(usePackrat())

  withr::local_envvar(RSCONNECT_PACKRAT = "FALSE")
  withr::local_options(rsconnect.packrat = TRUE)
  expect_false(usePackrat())
})

test_that("package_record works", {
  latin1Record <- package_record("latin1package", lib_dir = test_path("packages"))
  expect_equal(latin1Record$Author, "Jens Fröhling")

  utf8Record <- package_record("utf8package", lib_dir = test_path("packages"))
  expect_equal(utf8Record$Author, "Jens Fröhling")

  windows1251Record <- package_record("windows1251package", lib_dir = test_path("packages"))
  expect_equal(windows1251Record$Author, "Сергей Брин")
})
