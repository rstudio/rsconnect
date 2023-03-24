test_that("can snapshot deps with renv", {
  app_dir <- local_temp_app(list("foo.R" = "library(MASS)"))
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "MASS")

  # No renv lockfile left behind
  expect_equal(list.files(app_dir), "foo.R")
})

test_that("can snapshot deps with packrat", {
  withr::local_options(rsconnect.packrat = TRUE)
  app_dir <- local_temp_app(list("foo.R" = "library(MASS)"))
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "MASS")

  # No packrat lockfile left behind
  expect_equal(list.files(app_dir), "foo.R")
})

test_that("can capture deps from renv lockfile", {
  skip("why doesn't restore work?")
  withr::local_options(pkgType = "source") # speed up renv::restore()

  app_dir <- local_temp_app()
  file.copy(test_path("renv-recommended/renv.lock"), app_dir)
  renv::restore(app_dir, prompt = FALSE)
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "MASS")

  # No renv lockfile or directory left behind
  expect_equal(list.files(app_dir), character())
})

# -------------------------------------------------------------------------

test_that("error if can't find source", {
  mockr::local_mock(snapshotRenvDependencies = function(...) {
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

