test_that("can deploy to shinyapps.io and terminate", {
  app_name <- paste0(
    run_prefix,
    "-rsconnect-test-",
    strftime(Sys.time(), "%Y%m%d%H%M%S")
  )
  expect_no_error(
    deployApp(
      appDir = "example-shiny",
      appName = app_name,
      account = shinyapps_name,
      forceUpdate = TRUE,
      manifestPath = "example-shiny/manifest.json"
    )
  )
  expect_no_error(
    terminateApp(app_name, account = shinyapps_name)
  )
})
