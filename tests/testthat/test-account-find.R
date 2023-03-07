test_that("validates its arguments", {
  expect_snapshot(error = TRUE, {
    findAccount(1, NULL)
    findAccount(NULL, 1)
  })
})

test_that("error if no accounts", {
  mockr::local_mock(accounts = fakeAccounts(character(), character()))

  expect_snapshot(findAccount(), error = TRUE)
})

test_that("error if no matching account", {
  mockr::local_mock(accounts = fakeAccounts("name", "server"))

  expect_snapshot(error = TRUE, {
    findAccount("unknown", NULL)
    findAccount(NULL, "unknown")
    findAccount("unknown", "unknown")
  })
})

test_that("error if ambiguous accounts in non-interactive environment", {
  mockr::local_mock(accounts = fakeAccounts(c("a", "a", "b"), c("x", "y", "y")))

  expect_snapshot(error = TRUE, {
    findAccount()
    findAccount("a", NULL)
    findAccount(NULL, "y")
  })
})

test_that("prompted to pick account in interactive environment", {
  withr::local_options(
    rlang_interactive = TRUE,
    cli_prompt = "2"
  )
  mockr::local_mock(accounts = fakeAccounts(c("a", "a", "b"), c("x", "y", "y")))

  expect_snapshot({
    out <- findAccount()
  })
  expect_equal(out, list(name = "a", server = "y"))
})

test_that("returns account + server when uniquely identified", {
  mockr::local_mock(accounts = fakeAccounts("a", "x"))

  expect_equal(findAccount(NULL, NULL), list(name = "a", server = "x"))
  expect_equal(findAccount("a", NULL), list(name = "a", server = "x"))
  expect_equal(findAccount(NULL, "x"), list(name = "a", server = "x"))
  expect_equal(findAccount("a", "x"), list(name = "a", server = "x"))
})
