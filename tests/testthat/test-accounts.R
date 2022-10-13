context("accounts")

havingFakeAccounts <- function(expr) {
  registerUserApiKey("simple", "alice", 13, "alice-api-key")
  registerUserApiKey("complex", "hatter+mad@example.com", 42, "hatter-api-key")

  eval(expr)
}

test_that("account file returned with server name", {
  havingFakeConfig(
    havingFakeAccounts({
      expected <- normalizePath(file.path(rsconnectConfigDir("accounts"),
                                          "simple/alice.dcf"))
      dir <- accountConfigFile("alice", server = "simple")
      expect_equal(dir, expected)
    })
  )
})

test_that("account file containing pattern characters found with server name", {
  havingFakeConfig(
    havingFakeAccounts({
      # https://github.com/rstudio/rsconnect/issues/620
      expected <- normalizePath(file.path(rsconnectConfigDir("accounts"),
                                          "complex/hatter+mad@example.com.dcf"))
      dir <- accountConfigFile("hatter+mad@example.com", server = "complex")
      expect_equal(dir, expected)
    })
  )
})

test_that("account file found without server name", {
  havingFakeConfig(
    havingFakeAccounts({
      expected <- normalizePath(file.path(rsconnectConfigDir("accounts"),
                                          "simple/alice.dcf"))
      dir <- accountConfigFile("alice", server = NULL)
      expect_equal(dir, expected)
    })
  )
})

test_that("account file containing pattern characters found without server name", {
  havingFakeConfig(
    havingFakeAccounts({
      # https://github.com/rstudio/rsconnect/issues/620
      expected <- normalizePath(file.path(rsconnectConfigDir("accounts"),
                                          "complex/hatter+mad@example.com.dcf"))
      dir <- accountConfigFile("hatter+mad@example.com", server = NULL)
      expect_equal(dir, expected)
    })
  )
})
