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
  registerAccount("server", "1", "id", token = "token", secret = "SECRET")
  registerAccount("server", "2", "id", token = "token", private_key = "SECRET")
  registerAccount("server", "3", "id", apiKey = "SECRET")

  expect_snapshot({
    accountInfo("1")$secret
    accountInfo("2")$private_key
    accountInfo("3")$apiKey
  })
})

test_that("setAccountInfo() gives nice error on bad copy and paste", {
  expect_snapshot(setAccountInfo("name", "token", "<SECRET>"), error = TRUE)
})
