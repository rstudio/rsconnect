test_that("addConnectServer is deprecated", {
  skip_on_cran()
  skip_if_not_installed("webfakes")

  local_temp_config()

  service <- service_settings_200()
  url <- buildHttpUrl(service)

  expect_snapshot(
    addConnectServer(url, quiet = TRUE)
  )
})
