test_that("deployDoc correctly reports bad path", {
  expect_snapshot(deployDoc("doesntexist.Rmd"), error = TRUE)
})

# standardizeSingleDocDeployment ------------------------------------------

test_that("turns appDir into appDir + appPrimarySourceDoc", {
  dir <- local_temp_app(list("foo.R" = ""))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.R"))
  expect_equal(doc$appDir, normalizePath(dir))
  expect_equal(doc$appPrimaryDoc, "foo.R")
})

test_that("shiny rmd deploys whole directory", {
  dir <- local_temp_app(list("foo.Rmd" = c(
    "---",
    "runtime: shiny",
    "---"
  )))
  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.Rmd"))
  expect_equal(doc$appFiles, NULL)
})

test_that("regular rmd deploys file and dependencies", {
  dir <- local_temp_app(list(
    "foo.Rmd" = c(
      "---",
      "resource_files: [foo.csv]",
      "---"
    ),
    "foo.csv" = ""
  ))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.Rmd"), quiet = TRUE)
  expect_equal(doc$appFiles, c("foo.Rmd", "foo.csv"))
})

test_that("regular rmd deploys .Rprofile, if present", {
  dir <- local_temp_app(list(
    "foo.Rmd" = "",
    ".Rprofile" = ""
  ))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.Rmd"), quiet = TRUE)
  expect_equal(doc$appFiles, c("foo.Rmd", ".Rprofile"))
})


test_that("other types deploy that one file", {
  dir <- local_temp_app(list("foo.R" = ""))
  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.R"))
  expect_equal(doc$appFiles, "foo.R")
})
