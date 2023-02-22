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

test_that("deployHook executes function if set", {
  expect_equal(
    runDeploymentHook("PATH", "rsconnect.pre.deploy"),
    NULL
  )

  withr::local_options(rsconnect.pre.deploy = function(path) path)
  expect_equal(
    runDeploymentHook("PATH", "rsconnect.pre.deploy"),
    "PATH"
  )
  expect_snapshot(
    . <- runDeploymentHook("PATH", "rsconnect.pre.deploy", verbose = TRUE)
  )
})
