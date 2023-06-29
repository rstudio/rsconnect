test_that("can snapshot deps with renv", {
  app_dir <- local_temp_app(list("foo.R" = "library(MASS)"))
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "MASS")
  expect_named(pkgs$MASS, c("Source", "Repository", "description"))

  # No renv lockfile left behind
  expect_equal(list.files(app_dir), "foo.R")
})

test_that("can snapshot deps with packrat", {
  withr::local_options(rsconnect.packrat = TRUE)
  app_dir <- local_temp_app(list("foo.R" = "library(MASS)"))
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "MASS")
  expect_named(pkgs$MASS, c("Source", "Repository", "description"))

  # No packrat lockfile left behind
  expect_equal(list.files(app_dir), "foo.R")
})

test_that("can capture deps from renv lockfile", {
  withr::local_options(renv.verbose = FALSE)

  app_dir <- local_temp_app(list(foo.R = "library(foreign)"))
  renv::snapshot(app_dir, prompt = FALSE)
  expect_snapshot(pkgs <- bundlePackages(app_dir))
  expect_named(pkgs, "foreign")
  expect_named(pkgs$foreign, c("Source", "Repository", "description"))

  # No renv lockfile or directory left behind
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
