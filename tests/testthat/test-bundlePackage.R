test_that("errors if dependencies aren't installed", {
  app_dir <- withr::local_tempdir()
  writeLines(con = file.path(app_dir, "index.Rmd"), c(
    "```{r}",
    "library(doesntexist1)",
    "library(doesntexist2)",
    "```"
  ))

  expect_snapshot(
    bundlePackages(app_dir, appMode = "rmd-static", assetTypeName = "asset"),
    error = TRUE
  )
})
