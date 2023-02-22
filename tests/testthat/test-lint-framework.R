test_that("lints give have useful print method", {
  expect_snapshot(lint(test_path("shinyapp-with-absolute-paths")))
})

test_that("checkLayout() errors if primary doc & app.R", {
  dir <- local_temp_app(list(
    "app.R" = "",
    "myscript.R" = ""
  ))

  expect_snapshot(checkAppLayout(dir, "myscript.R"), error = TRUE)
})

test_that("checkLayout fails if no known structure", {
  dir <- local_temp_app(list(
    "data.txt" = "",
    "cats.csv" = ""
  ))

  expect_snapshot(checkAppLayout(dir), error = TRUE)
})

test_that("checkLayout succeeds with some common app structures", {
  rmd <- local_temp_app(list("foo.Rmd" = ""))
  expect_no_error(checkAppLayout(rmd))

  shiny1 <- local_temp_app(list("app.R" = ""))
  expect_no_error(checkAppLayout(rmd))

  shiny2 <- local_temp_app(list("server.R" = "", "ui.R" = ""))
  expect_no_error(checkAppLayout(rmd))

  static <- local_temp_app(list("foo.html" = ""))
  expect_no_error(checkAppLayout(rmd))
})
