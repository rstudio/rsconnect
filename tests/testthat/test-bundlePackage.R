test_that("can snapshot deps with renv", {
  app_dir <- local_temp_app(list("foo.R" = "library(MASS)"))
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "MASS")

  # No renv lockfile left behind
  expect_equal(list.files(app_dir), c("foo.R", "packrat"))
})

test_that("can snapshot deps with packrat", {
  withr::local_options(rsconnect.packrat = TRUE)
  app_dir <- local_temp_app(list("foo.R" = "library(MASS)"))
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "MASS")

  # No packrat lockfile left behind
  expect_equal(list.files(app_dir), c("foo.R", "packrat"))
})

test_that("can capture deps from renv lockfile", {
  app_dir <- local_temp_app()
  file.copy(test_path("renv-recommended/renv.lock"), app_dir)
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "MASS")

  # No renv lockfile left behind
  expect_equal(list.files(app_dir), "packrat")
})

# -------------------------------------------------------------------------

test_that("returns list of package details and copies descriptions", {
  withr::local_options(rsconnect.packrat = TRUE)
  app_dir <- local_temp_app(list("foo.Rmd" = ""))
  out <- bundlePackages(app_dir, quiet = TRUE)
  expect_type(out, "list")

  common <- c("Source", "Repository", "description")
  expect_equal(setdiff(common, names(out[[1]])), character())

  expect_setequal(
    names(out),
    list.files(file.path(app_dir, "packrat", "desc"))
  )
})

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

