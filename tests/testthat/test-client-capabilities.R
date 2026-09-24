test_that("serverDisplayName() names each server", {
  expect_equal(serverDisplayName(fake_client("connectClient")), "Posit Connect")
  expect_equal(
    serverDisplayName(fake_client("shinyAppsClient")),
    "shinyapps.io"
  )
  expect_equal(
    serverDisplayName(fake_client("connectCloudClient")),
    "Posit Connect Cloud"
  )
})

test_that("supportsEnvVars() is correct for each client", {
  expect_true(supportsEnvVars(fake_client("connectClient")))
  expect_false(supportsEnvVars(fake_client("shinyAppsClient")))
  expect_true(supportsEnvVars(fake_client("connectCloudClient")))
})

test_that("supportsEnvVarManagement() is correct for each client", {
  expect_true(supportsEnvVarManagement(fake_client("connectClient")))
  expect_false(supportsEnvVarManagement(fake_client("shinyAppsClient")))
  expect_false(supportsEnvVarManagement(fake_client("connectCloudClient")))
})

test_that("supportsNodejs() is correct for each client", {
  expect_true(supportsNodejs(fake_client("connectClient")))
  expect_false(supportsNodejs(fake_client("shinyAppsClient")))
  expect_false(supportsNodejs(fake_client("connectCloudClient")))
})

test_that("supportsUserManagement() is correct for each client", {
  expect_false(supportsUserManagement(fake_client("connectClient")))
  expect_true(supportsUserManagement(fake_client("shinyAppsClient")))
  expect_true(supportsUserManagement(fake_client("connectCloudClient")))
})

test_that("requiresUpload() is correct for each client", {
  expect_true(requiresUpload(fake_client("connectClient")))
  expect_false(requiresUpload(fake_client("shinyAppsClient")))
  expect_false(requiresUpload(fake_client("connectCloudClient")))
})

test_that("pythonEnabledByDefault() is correct for each client", {
  expect_true(pythonEnabledByDefault(fake_client("connectClient")))
  expect_false(pythonEnabledByDefault(fake_client("shinyAppsClient")))
  expect_true(pythonEnabledByDefault(fake_client("connectCloudClient")))
})

test_that("supportsVisibility() is correct for each client", {
  expect_false(supportsVisibility(fake_client("connectClient")))
  expect_true(supportsVisibility(fake_client("shinyAppsClient")))
  expect_false(supportsVisibility(fake_client("connectCloudClient")))
})

test_that("supportsMetadataSync() is correct for each client", {
  expect_true(supportsMetadataSync(fake_client("connectClient")))
  expect_true(supportsMetadataSync(fake_client("shinyAppsClient")))
  expect_false(supportsMetadataSync(fake_client("connectCloudClient")))
})

test_that("staticRmdNeedsShiny() is correct for each client", {
  expect_false(staticRmdNeedsShiny(fake_client("connectClient")))
  expect_true(staticRmdNeedsShiny(fake_client("shinyAppsClient")))
  expect_false(staticRmdNeedsShiny(fake_client("connectCloudClient")))
})

test_that("addsUtmParameters() is correct for each client", {
  expect_false(addsUtmParameters(fake_client("connectClient")))
  expect_false(addsUtmParameters(fake_client("shinyAppsClient")))
  expect_true(addsUtmParameters(fake_client("connectCloudClient")))
})
