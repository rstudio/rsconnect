context("type detection")

test_that("Shiny R Markdown files are detected correctly", {
  expect_true(isShinyRmd("./shiny-rmds/shiny-rmd-dashes.Rmd"))
  expect_true(isShinyRmd("./shiny-rmds/shiny-rmd-dots.Rmd"))
  expect_false(isShinyRmd("./shiny-rmds/non-shiny-rmd.Rmd"))
})
