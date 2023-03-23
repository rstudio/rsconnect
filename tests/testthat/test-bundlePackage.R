test_that("non-R apps don't have packages", {
  withr::local_options(rsconnect.packrat = TRUE)
  app_dir <- local_temp_app(list(index.html = ""))
  out <- bundlePackages(app_dir, appMode = "static", quiet = TRUE)
  expect_equal(out, list())
})

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
  withr::local_options(rsconnect.packrat = TRUE)
  mockr::local_mock(snapshotPackratDependencies = function(...) {
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
    . <- bundlePackages(app_dir, appMode = "rmd-static"),
    error = TRUE
  )
})
