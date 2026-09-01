test_that("showUsers returns a data frame for PCC", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        list(
          list(
            user = list(
              id = "user-uuid-1",
              email = "alice@example.com",
              display_name = "Alice Smith"
            ),
            role = "collaborator"
          ),
          list(
            user = list(
              id = "user-uuid-2",
              email = "bob@example.com",
              display_name = "Bob Jones"
            ),
            role = "viewer"
          )
        )
      }
    )
  })

  result <- showUsers(
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$email, c("alice@example.com", "bob@example.com"))
  expect_equal(result$display_name, c("Alice Smith", "Bob Jones"))
  expect_equal(result$role, c("collaborator", "viewer"))
  expect_true(all(is.na(result$account)))
})

test_that("removeAuthorizedUser calls removeApplicationUser on PCC", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  removed_user_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        list(list(user = list(id = "user-uuid-1", email = "alice@example.com")))
      },
      removeApplicationUser = function(appId, userId) {
        removed_user_id <<- userId
        invisible(TRUE)
      }
    )
  })

  removeAuthorizedUser(
    "alice@example.com",
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(removed_user_id, "user-uuid-1")
})

test_that("showUsers returns empty data frame with correct columns when no users authorized", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) list()
    )
  })

  result <- showUsers(
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("id", "email", "account", "display_name", "role"))
})

test_that("showUsers on shinyapps.io returns only id/email/account columns (no display_name/role)", {
  local_temp_config()
  addTestServer(url = "https://shinyapps.io", name = "shinyapps.io")
  addTestAccount("myaccount", server = "shinyapps.io")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = 42L))
      },
      listApplicationAuthorization = function(appId) {
        list(
          list(
            user = list(id = "101", email = "alice@example.com"),
            account = "alice-account"
          )
        )
      }
    )
  })

  result <- showUsers(
    appName = "myapp",
    account = "myaccount",
    server = "shinyapps.io"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_named(result, c("id", "email", "account"))
  expect_equal(result$account, "alice-account")
})

test_that("showUsers names shinyapps.io (not Connect Cloud) in the malformed-record error", {
  local_temp_config()
  addTestServer(url = "https://shinyapps.io", name = "shinyapps.io")
  addTestAccount("myaccount", server = "shinyapps.io")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = 42L))
      },
      listApplicationAuthorization = function(appId) {
        # record with neither id nor email — unexpected shape
        list(list(user = list()))
      }
    )
  })

  expect_error(
    showUsers(
      appName = "myapp",
      account = "myaccount",
      server = "shinyapps.io"
    ),
    regexp = "Unexpected response from shinyapps\\.io"
  )
})

test_that("addAuthorizedUser calls inviteApplicationUser on PCC", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  invited <- list()
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      inviteApplicationUser = function(appId, email, sendEmail, emailMessage) {
        invited[[length(invited) + 1]] <<- list(appId = appId, email = email)
        invisible(TRUE)
      }
    )
  })

  addAuthorizedUser(
    "alice@example.com",
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_length(invited, 1)
  expect_equal(invited[[1]]$email, "alice@example.com")
})

test_that("showInvited maps PCC email_address/is_expired fields", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationInvitations = function(appId) {
        list(list(
          id = "invite-uuid-1",
          email_address = "alice@example.com",
          is_expired = FALSE
        ))
      }
    )
  })

  result <- showInvited(
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(result$email, "alice@example.com")
  expect_true(is.na(result$link))
  expect_false(result$expired)
})

test_that("addAuthorizedUser warns when sendEmail is non-NULL on PCC", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      inviteApplicationUser = function(appId, email, sendEmail, emailMessage) {
        invisible(TRUE)
      }
    )
  })

  msgs <- character(0)
  withCallingHandlers(
    addAuthorizedUser(
      "alice@example.com",
      appDir = app_dir,
      account = "myaccount",
      server = "connect.posit.cloud",
      sendEmail = FALSE
    ),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl(
    "sendEmail.*ignored|ignored.*sendEmail|PCC always",
    msgs
  )))
})

test_that("showInvited returns NA for invitation records missing email and expired fields", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationInvitations = function(appId) {
        # record with neither email_address/email nor is_expired/expired
        list(list(id = "invite-uuid-missing"))
      }
    )
  })

  result <- showInvited(
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$email))
  expect_true(is.na(result$expired))
})

test_that("showUsers aborts with a clear message when a user record has neither id nor email", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        # record with no user$id or user$email fields — unexpected shape
        list(list(user = list()))
      }
    )
  })

  # The abort is raised inside showUsers_impl.
  expect_error(
    showUsers(
      appDir = app_dir,
      account = "myaccount",
      server = "connect.posit.cloud"
    ),
    regexp = "Unexpected response from Posit Connect Cloud"
  )
})

test_that("showUsers errors on a non-shinyapps, non-PCC server", {
  local_temp_config()
  addTestServer(
    url = "https://connect.example.com",
    name = "connect.example.com"
  )
  addTestAccount("myaccount", server = "connect.example.com")

  expect_error(
    showUsers(
      appName = "myapp",
      account = "myaccount",
      server = "connect.example.com"
    ),
    regexp = "shinyapps\\.io"
  )
})

test_that("resendInvitation calls resendApplicationInvitation on PCC", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  resent_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationInvitations = function(appId) {
        list(list(
          id = "invite-uuid-1",
          email_address = "alice@example.com",
          is_expired = FALSE
        ))
      },
      resendApplicationInvitation = function(inviteId, regenerate) {
        resent_id <<- inviteId
        invisible(TRUE)
      }
    )
  })

  resendInvitation(
    "alice@example.com",
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(resent_id, "invite-uuid-1")
})

test_that("resolveContentTarget uses deployment-record appId on PCC, not title", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "my-content",
    appId = "content-uuid-abc",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  captured_app_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        captured_app_id <<- appId
        list()
      }
    )
  })

  showUsers(
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(captured_app_id, "content-uuid-abc")
})

test_that("contentId targets PCC content directly without a deployment record", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  # No addTestDeployment — contentId must not require a local record.

  captured_app_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        captured_app_id <<- appId
        list()
      }
    )
  })

  showUsers(
    appDir = app_dir,
    contentId = "content-uuid-direct",
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(captured_app_id, "content-uuid-direct")
})

test_that("contentId is rejected on shinyapps.io", {
  local_temp_config()
  addTestServer(url = "https://shinyapps.io", name = "shinyapps.io")
  addTestAccount("myaccount", server = "shinyapps.io")

  expect_error(
    showUsers(
      contentId = "content-uuid-direct",
      account = "myaccount",
      server = "shinyapps.io"
    ),
    regexp = "only supported on Posit Connect Cloud"
  )
})

test_that("resolveContentTarget aborts with clear message on PCC when no deployment record", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  # No addTestDeployment — no record exists

  expect_error(
    suppressWarnings(
      showUsers(
        appDir = app_dir,
        account = "myaccount",
        server = "connect.posit.cloud"
      )
    ),
    regexp = "No deployment record found"
  )
})

test_that("resolveContentTarget: appName selects correct record among multiple in appDir", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "a",
    appId = "content-uuid-aaa",
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  addTestDeployment(
    app_dir,
    appName = "b",
    appId = "content-uuid-bbb",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  captured_app_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        captured_app_id <<- appId
        list()
      }
    )
  })

  showUsers(
    appDir = app_dir,
    appName = "b",
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(captured_app_id, "content-uuid-bbb")
})

test_that("resolveContentTarget: omitting appName with multiple records lists candidates non-interactively", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "a",
    appId = "content-uuid-aaa",
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  addTestDeployment(
    app_dir,
    appName = "b",
    appId = "content-uuid-bbb",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  expect_error(
    suppressWarnings(
      showUsers(
        appDir = app_dir,
        account = "myaccount",
        server = "connect.posit.cloud"
      )
    ),
    regexp = "appName|disambiguate|Known"
  )
})

test_that("resolveContentTarget delegates to resolveApplication on shinyapps.io", {
  local_temp_config()
  addTestServer(url = "https://shinyapps.io", name = "shinyapps.io")
  addTestAccount("myaccount", server = "shinyapps.io")

  captured_app_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = 42L))
      },
      listApplicationAuthorization = function(appId) {
        captured_app_id <<- appId
        list()
      }
    )
  })

  showUsers(
    appName = "myapp",
    account = "myaccount",
    server = "shinyapps.io"
  )
  expect_equal(captured_app_id, 42L)
})

test_that("removeAuthorizedUser resolves content target exactly once (no double prompt)", {
  # Regression test for double resolveContentTarget() via public showUsers().
  # Before the fix, removeAuthorizedUser() called resolveContentTarget() and
  # then showUsers() called it again — two independent prompts on multi-record
  # appDir, potentially acting on different content. After the fix, clientForAccount
  # is built once and showUsers_impl() is called directly with the resolved id.
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "a",
    appId = "content-uuid-aaa",
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  addTestDeployment(
    app_dir,
    appName = "b",
    appId = "content-uuid-bbb",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  client_build_count <- 0L
  removed_app_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    client_build_count <<- client_build_count + 1L
    list(
      listApplicationAuthorization = function(appId) {
        list(list(user = list(id = "user-uuid-1", email = "alice@example.com")))
      },
      removeApplicationUser = function(appId, userId) {
        removed_app_id <<- appId
        invisible(TRUE)
      }
    )
  })

  removeAuthorizedUser(
    "alice@example.com",
    appDir = app_dir,
    appName = "b",
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  # clientForAccount built exactly once → content resolved exactly once
  expect_equal(client_build_count, 1L)
  # removeApplicationUser called with "b"'s appId, not "a"'s
  expect_equal(removed_app_id, "content-uuid-bbb")
})

test_that("removeAuthorizedUser resolves by UUID id on PCC (not email-only fallback)", {
  # Regression: is.numeric("uuid-string") == FALSE, so the old code fell to the
  # email branch and never matched PCC UUID ids.
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  removed_user_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        list(list(
          user = list(id = "user-uuid-abc", email = "alice@example.com")
        ))
      },
      removeApplicationUser = function(appId, userId) {
        removed_user_id <<- userId
        invisible(TRUE)
      }
    )
  })

  removeAuthorizedUser(
    "user-uuid-abc",
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(removed_user_id, "user-uuid-abc")
})

test_that("removeAuthorizedUser matches by email when another record has no email", {
  # Regression: a co-listed record with a redacted (NA) email made the logical
  # subset `users[users$email == user, ]` include a phantom NA row, so the later
  # `is.na(user$id)` check saw length > 1 and errored. which() drops the NA.
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  removed_user_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        list(
          list(user = list(id = "id-alice", email = "alice@example.com")),
          # A second member whose email is redacted (absent) on PCC.
          list(user = list(id = "id-redacted", email = NULL))
        )
      },
      removeApplicationUser = function(appId, userId) {
        removed_user_id <<- userId
        invisible(TRUE)
      }
    )
  })

  removeAuthorizedUser(
    "alice@example.com",
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(removed_user_id, "id-alice")
})

test_that("resendInvitation resolves by UUID invite id on PCC (not email-only fallback)", {
  # Same is.numeric() bug as removeAuthorizedUser: UUID invite ids are character,
  # so the old code fell to the email branch and never matched by id.
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  resent_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationInvitations = function(appId) {
        list(list(
          id = "invite-uuid-xyz",
          email_address = "bob@example.com",
          is_expired = FALSE
        ))
      },
      resendApplicationInvitation = function(inviteId, regenerate) {
        resent_id <<- inviteId
        invisible(TRUE)
      }
    )
  })

  resendInvitation(
    "invite-uuid-xyz",
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_equal(resent_id, "invite-uuid-xyz")
})

test_that("removeAuthorizedUser aborts with clear message when matched user has no id", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        # user record has email but no id field — id will be NA after showUsers_impl
        list(list(user = list(email = "alice@example.com")))
      }
    )
  })

  expect_error(
    removeAuthorizedUser(
      "alice@example.com",
      appDir = app_dir,
      account = "myaccount",
      server = "connect.posit.cloud"
    ),
    regexp = "no id"
  )
})

test_that("removeAuthorizedUser hints at redaction when the user cannot be matched", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      # PCC redacted the email, so matching on the caller's email fails.
      listApplicationAuthorization = function(appId) {
        list(list(user = list(id = "user-uuid-1", email = "REDACTED")))
      }
    )
  })

  expect_error(
    removeAuthorizedUser(
      "alice@example.com",
      appDir = app_dir,
      account = "myaccount",
      server = "connect.posit.cloud"
    ),
    regexp = "redacted"
  )
})

test_that("removeAuthorizedUser omits the redaction hint for an id lookup", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationAuthorization = function(appId) {
        list(list(user = list(id = "user-uuid-1", email = "alice@example.com")))
      }
    )
  })

  # An id-based lookup that misses is a plain not-found: the redaction hint
  # (which only helps email searches) must not appear.
  cnd <- suppressWarnings(
    expect_error(
      removeAuthorizedUser(
        "user-uuid-missing",
        appDir = app_dir,
        account = "myaccount",
        server = "connect.posit.cloud"
      ),
      regexp = "not found"
    )
  )
  expect_false(grepl("redacted", conditionMessage(cnd)))
})

test_that("resendInvitation aborts with clear message when matched invitation has no id", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplicationInvitations = function(appId) {
        # invitation record has email_address but no id — id will be NA after showInvited_impl
        list(list(email_address = "alice@example.com", is_expired = FALSE))
      }
    )
  })

  expect_error(
    resendInvitation(
      "alice@example.com",
      appDir = app_dir,
      account = "myaccount",
      server = "connect.posit.cloud"
    ),
    regexp = "no id"
  )
})

test_that("cleanupPasswordFile is NOT called on PCC accounts", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  app_dir <- withr::local_tempdir()
  addTestDeployment(
    app_dir,
    appName = "myapp",
    appId = "content-uuid-123",
    account = "myaccount",
    server = "connect.posit.cloud"
  )

  cleanup_called <- FALSE
  local_mocked_bindings(
    cleanupPasswordFile = function(...) {
      cleanup_called <<- TRUE
      invisible(TRUE)
    },
    clientForAccount = function(...) {
      list(
        inviteApplicationUser = function(...) invisible(TRUE)
      )
    }
  )

  addAuthorizedUser(
    "alice@example.com",
    appDir = app_dir,
    account = "myaccount",
    server = "connect.posit.cloud"
  )
  expect_false(cleanup_called)
})
