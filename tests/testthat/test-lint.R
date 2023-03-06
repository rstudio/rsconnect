test_that("lints give have useful print method", {
  expect_snapshot({
    lint(test_path("test-rmd-bad-case"))
    lint(test_path("shinyapp-appR"))
  })
})

test_that("The linter believes that the Shiny example apps are okay", {

  examples <- list.files(system.file("examples", package = "shiny"), full.names = TRUE)
  if (length(examples)) {

    results <- lapply(examples, lint)

    expect_output(lints <- lapply(results, print))
    lapply(lints, function(project) {
      lapply(project, function(file) {
        lapply(file, function(linter) {
          expect_true(length(linter$indices) == 0)
        })
      })
    })
  }
})
