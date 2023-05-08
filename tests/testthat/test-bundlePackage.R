test_that("can snapshot deps with renv", {
  dir <- local_temp_app(foo.R = "library(MASS)")
  expect_snapshot(pkgs <- bundlePackages(dir))
  expect_named(pkgs, "MASS")

  # No renv lockfile left behind
  expect_equal(list.files(dir), "foo.R")
})

test_that("can snapshot deps with packrat", {
  withr::local_options(rsconnect.packrat = TRUE)
  dir <- local_temp_app(foo.R = "library(MASS)")
  expect_snapshot(pkgs <- bundlePackages(dir))
  expect_named(pkgs, "MASS")

  # No packrat lockfile left behind
  expect_equal(list.files(dir), "foo.R")
})

test_that("can capture deps from renv lockfile", {
  withr::local_options(renv.verbose = FALSE)

  dir <- local_temp_app(foo.R = "library(foreign)")
  renv::snapshot(dir, prompt = FALSE)
  expect_snapshot(pkgs <- bundlePackages(dir))
  expect_named(pkgs, "foreign")

  # No renv lockfile or directory left behind
  expect_equal(list.files(dir), "foo.R")
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

  dir <- local_temp_app(index.Rmd = c(
    "```{r}",
    "library(shiny)",
    "```"
  ))

  expect_snapshot(
    . <- bundlePackages(dir),
    error = TRUE
  )
})
