test_that("deploy does not error", {
  # Also test verbose logging
  expect_true(deployApp(
    "example-shiny",
    appTitle = "Test",
    account = account,
    logLevel = "verbose"
  ))
})

test_that("re-deploy does not error", {
  # Let's exercise the env vars as well
  expect_true(deployApp("example-shiny", envVars = "TEST", account = account))
})

test_that("listAccountEnvVars", {
  envs <- listAccountEnvVars(account = account)
  expect_true(nrow(envs) == 1)
})

test_that("deploy nodejs does not error", {
  if (nodejs_supported(account)) {
    expect_true(
      deployApp("example-nodejs", account = account)
    )
    # redeploy also succeeds
    expect_true(deployApp("example-nodejs", account = account))
  } else {
    expect_error(
      deployApp("example-nodejs", account = account),
      "2026.04.0"
    )
  }
})
