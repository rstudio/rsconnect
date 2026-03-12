app_name <- paste0("rsconnect-test-", strftime(Sys.time(), "%Y%m%d%H%M%S"))

test_that("deploy to shinyapps.io does not error", {
  expect_no_error(
    deployApp(
      appDir = "example-shiny",
      appName = app_name,
      account = shinyapps_name,
      forceUpdate = TRUE,
      manifestPath = "example-shiny/manifest.json"
    )
  )
})

test_that("terminate app on shinyapps.io", {
  expect_no_error(
    terminateApp(app_name, account = shinyapps_name)
  )
})
