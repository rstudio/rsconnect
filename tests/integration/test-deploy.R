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

test_that("redeploy adds the content guid to a record without one", {
  appDir <- test_path("deploy-content", "example-shiny")
  appName <- paste0(run_prefix, "-legacy-record")
  expect_true(deployApp(appDir, appName = appName, account = account))

  record <- deployments(appDir, nameFilter = appName, accountFilter = account)
  guid <- record$appGuid
  expect_match(guid, "-")
  # deployments() reports the guid as appId; the stored appId is numeric
  expect_equal(record$appId, guid)
  stored <- read.dcf(record$deploymentFile)
  expect_match(stored[, "appId"], "^[0-9]+$")

  # Rewrite the record the way an rsconnect version that did not store the
  # guid would have saved it
  write.dcf(
    stored[, colnames(stored) != "appGuid", drop = FALSE],
    record$deploymentFile,
    width = 4096
  )
  legacy <- deployments(appDir, nameFilter = appName, accountFilter = account)
  expect_true(is.na(legacy$appGuid))
  expect_equal(legacy$appId, stored[, "appId"][[1]])

  # Redeploying finds the same content by its numeric id and stores the guid
  expect_true(deployApp(appDir, appName = appName, account = account))
  migrated <- deployments(appDir, nameFilter = appName, accountFilter = account)
  expect_equal(migrated$appGuid, guid)
  expect_equal(migrated$appId, guid)
  expect_equal(migrated$url, record$url)

  # The RStudio IDE passes the appId from deployments() to getAppById()
  app <- rsconnect:::getAppById(guid, account, account, server)
  expect_equal(app$guid, guid)
  expect_equal(app$url, record$url)

  # A redeploy from the migrated record updates the same content
  expect_true(deployApp(appDir, appName = appName, account = account))
  expect_equal(
    deployments(appDir, nameFilter = appName, accountFilter = account)$appGuid,
    guid
  )
})

# Deploy every piece of sample content under deploy-content/ to a real Posit
# Connect server and assert each deploy succeeds
for (fixture in fixtures()) {
  test_that(paste("deployApp() succeeds:", fixture), {
    expect_true(deployApp(
      test_path("deploy-content", fixture),
      appName = paste0(run_prefix, "-", fixture),
      account = account
    ))
  })
}
