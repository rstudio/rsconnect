test_that("All hosted product names are identified as cloud", {
  expect_true(isCloudServer("shinyapps.io"))
  expect_true(isCloudServer("rstudio.cloud"))
  expect_true(isCloudServer("posit.cloud"))
  expect_false(isCloudServer("connect.internal"))
})

test_that("no accounts returns empty data frame", {
  local_temp_config()

  expect_equal(
    accounts(),
    data.frame(name = character(), server = character(), stringsAsFactors = FALSE)
  )
})

test_that("secrets are hidden from casual inspection", {
  local_temp_config()
  registerUserToken("example.com", "john", "userId", "token", "THIS IS A SECRET")
  registerCloudTokenSecret(
    "shinyapps.io",
    "susan",
    "userId",
    "accountId",
    "token",
    "THIS IS A SECRET"
  )

  expect_snapshot({
    accountInfo("john")$private_key
    accountInfo("susan")$secret

    str(accountInfo("john"))
  })

})
