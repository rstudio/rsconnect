test_that("deployTFModel() aborts on Connect Cloud", {
  skip_on_cran()
  appDir <- local_temp_app(list("saved_model.pb" = ""))
  local_pcc_deploy_env(appDir, appId = NULL)

  expect_error(
    deployTFModel(
      appDir,
      appName = "tf-model",
      account = "myaccount",
      server = "connect.posit.cloud",
      logLevel = "quiet",
      lint = FALSE,
      launch.browser = FALSE
    ),
    regexp = "'tensorflow-saved-model' is not supported by Connect Cloud"
  )
})
