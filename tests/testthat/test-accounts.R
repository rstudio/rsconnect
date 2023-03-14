test_that("no accounts returns empty data frame", {
  local_temp_config()

  expect_equal(
    accounts(),
    data.frame(name = character(), server = character(), stringsAsFactors = FALSE)
  )
})

test_that("hasAccounts works", {
  local_temp_config()
  addTestServer()
  addTestAccount("john")

  expect_true(hasAccount("john", "example.com"))
  expect_false(hasAccount("john", "example2.com"))
  expect_false(hasAccount("mary", "example.com"))
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

test_that("setAccountInfo() gives nice error on bad copy and paste", {
  expect_snapshot(setAccountInfo("name", "token", "<SECRET>"), error = TRUE)
})
