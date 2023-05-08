test_that("multiplication works", {
  app <- local_temp_app(list(
    "index.Rmd" = c(
      "---",
      "title: rmd",
      "---",
      "",
      "```{r}",
      "Sys.getenv('MYSQL_USER')",
      "```"
    )
  ))

  dryRun(app)
})

# userEnvVars -------------------------------------------------------------

test_that("ignores non-existent file", {
  expect_equal(userEnvVars("DOESNTEXIST"), character())
})

test_that("can parse simple .Renviron", {
  path <- withr::local_tempfile(lines = c(
    "# a comment",
    "",
    "A=1",
    "B=2"
  ))
  expect_equal(userEnvVars(path), c("A", "B"))
})

test_that("removes duplicates", {
  path <- withr::local_tempfile(lines = c(
    "# a comment",
    "",
    "A=1",
    "A=2"
  ))
  expect_equal(userEnvVars(path), "A")
})

test_that("not troubled by wrong number of equals", {
  path <- withr::local_tempfile(lines = c(
    "# a comment",
    "",
    "A",
    "B=1",
    "C=2=3"
  ))
  expect_equal(userEnvVars(path), c("B", "C"))
})

