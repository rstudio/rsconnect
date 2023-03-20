test_that("validates its arguments", {
  expect_snapshot(error = TRUE, {
    findAccount(1, NULL)
    findAccount(NULL, 1)
  })
})

test_that("error if no accounts", {
  local_temp_config()
  expect_snapshot(findAccount(), error = TRUE)
})

test_that("error if no matching account", {
  local_temp_config()
  addTestServer()
  addTestAccount("albert")

  expect_snapshot(error = TRUE, {
    findAccount("unknown", NULL)
    findAccount(NULL, "unknown")
    findAccount("unknown", "unknown")
  })
})

test_that("error if ambiguous accounts in non-interactive environment", {
  local_temp_config()
  addTestServer("a")
  addTestServer("b")
  addTestAccount("a", "x")
  addTestAccount("a", "y")
  addTestAccount("b", "y")

  expect_snapshot(error = TRUE, {
    findAccount()
    findAccount("a", NULL)
    findAccount(NULL, "y")
  })
})

test_that("prompted to pick account in interactive environment", {
  local_temp_config()
  addTestServer("a")
  addTestServer("b")
  addTestAccount("a", "x")
  addTestAccount("a", "y")
  addTestAccount("b", "y")

  simulate_user_input(2)
  expect_snapshot(out <- findAccount())
  expect_equal(out, list(name = "a", server = "y"))
})

test_that("returns account + server when uniquely identified", {
  local_temp_config()
  addTestServer("a")
  addTestAccount("a", "x")

  expect_equal(findAccount(NULL, NULL), list(name = "a", server = "x"))
  expect_equal(findAccount("a", NULL), list(name = "a", server = "x"))
  expect_equal(findAccount(NULL, "x"), list(name = "a", server = "x"))
  expect_equal(findAccount("a", "x"), list(name = "a", server = "x"))
})
