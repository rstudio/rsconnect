test_that("deployDoc correctly reports bad path", {
  skip_on_cran()
  expect_snapshot(deployDoc("doesntexist.Rmd"), error = TRUE)
})

# standardizeSingleDocDeployment ------------------------------------------

test_that("turns appDir into appDir + appPrimarySourceDoc", {
  skip_on_cran()
  dir <- local_temp_app(list("foo.R" = ""))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.R"))
  expect_equal(doc$appDir, normalizePath(dir))
  expect_equal(doc$appPrimaryDoc, "foo.R")
})

test_that("shiny rmd deploys whole directory", {
  skip_on_cran()
  dir <- local_temp_app(list("foo.Rmd" = c(
    "---",
    "runtime: shiny",
    "---"
  )))
  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.Rmd"))
  expect_equal(doc$appFiles, NULL)
})

test_that("regular rmd deploys file and dependencies", {
  skip_on_cran()
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
  skip_on_cran()
  dir <- local_temp_app(list(
    "foo.Rmd" = "",
    ".Rprofile" = ""
  ))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.Rmd"), quiet = TRUE)
  expect_equal(doc$appFiles, c("foo.Rmd", ".Rprofile"))
})

test_that("regular rmd deploys requirements.txt, if present", {
  skip_on_cran()
  dir <- local_temp_app(list(
    "foo.Rmd" = "",
    "requirements.txt" = ""
  ))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.Rmd"), quiet = TRUE)
  expect_equal(doc$appFiles, c("foo.Rmd", "requirements.txt"))
})

test_that("regular rmd deploys renv.lock, if present", {
  skip_on_cran()
  dir <- local_temp_app(list(
    "foo.Rmd" = "",
    "renv.lock" = ""
  ))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.Rmd"), quiet = TRUE)
  expect_equal(doc$appFiles, c("foo.Rmd", "renv.lock"))
})

test_that("regular html does not deploy .Rprofile", {
  skip_on_cran()
  dir <- local_temp_app(list(
    "foo.html" = "",
    ".Rprofile" = ""
  ))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.html"), quiet = TRUE)
  expect_equal(doc$appFiles, c("foo.html"))
})

test_that("regular html does not deploy requirements.txt", {
  skip_on_cran()
  dir <- local_temp_app(list(
    "foo.html" = "",
    "requirements.txt" = ""
  ))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.html"), quiet = TRUE)
  expect_equal(doc$appFiles, c("foo.html"))
})

test_that("regular html does not deploy renv.lock", {
  skip_on_cran()
  dir <- local_temp_app(list(
    "foo.html" = "",
    "renv.lock" = ""
  ))

  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.html"), quiet = TRUE)
  expect_equal(doc$appFiles, c("foo.html"))
})

test_that("other types deploy that one file", {
  skip_on_cran()
  dir <- local_temp_app(list("foo.R" = ""))
  doc <- standardizeSingleDocDeployment(file.path(dir, "foo.R"))
  expect_equal(doc$appFiles, "foo.R")
})
