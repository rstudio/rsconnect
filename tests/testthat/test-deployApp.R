test_that("needsVisibilityChange() returns FALSE when no change needed", {

  dummyApp <- function(visibility) {
    list(
      deployment = list(
        properties = list(
          application.visibility = visibility
        )
      )
    )
  }

  expect_false(needsVisibilityChange("connect.com"))
  expect_false(needsVisibilityChange("shinyapps.io", dummyApp("public"), NULL))
  expect_false(needsVisibilityChange("shinyapps.io", dummyApp("public"), "public"))
  expect_true(needsVisibilityChange("shinyapps.io", dummyApp(NULL), "private"))
  expect_true(needsVisibilityChange("shinyapps.io", dummyApp("public"), "private"))
})
