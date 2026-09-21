test_that("deploy does not error", {
  # Also test verbose logging
  expect_true(deployApp(
    test_path("deploy-content", "example-shiny"),
    appTitle = "Test",
    account = account,
    logLevel = "verbose"
  ))
})

test_that("re-deploy does not error", {
  # Let's exercise the env vars as well
  expect_true(deployApp(
    test_path("deploy-content", "example-shiny"),
    envVars = "TEST",
    account = account
  ))
})

test_that("listAccountEnvVars", {
  envs <- listAccountEnvVars(account = account)
  expect_true(nrow(envs) == 1)
})

# Deploy every piece of sample content under deploy-content/ to a real Posit
# Connect server and assert each deploy succeeds
for (deploy_fixture in deploy_fixtures()) {
  test_that(paste("deployApp() succeeds:", deploy_fixture), {
    expect_true(deployApp(
      test_path("deploy-content", deploy_fixture),
      appName = paste0(run_prefix, "-", deploy_fixture),
      account = account
    ))
  })
}
