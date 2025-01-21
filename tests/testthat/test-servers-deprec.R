test_that("addConnectServer is deprecated", {
  skip_on_cran()
  local_temp_config()

  expect_snapshot(
    addConnectServer("https://connect.posit.it/", quiet = TRUE)
  )
})
