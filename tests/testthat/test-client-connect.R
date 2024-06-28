test_that("leading timestamps are stripped", {
  expect_snapshot(
    stripConnectTimestamps(
      c(
        "2024/04/24 13:08:04.901698921 [rsc-session] Content GUID: 3bfbd98a-6d6d-41bd-a15f-cab52025742f",
        "2024/04/24 13:08:04.901734307 [rsc-session] Content ID: 43888",
        "2024/04/24 13:08:04.901742487 [rsc-session] Bundle ID: 94502",
        "2024/04/24 13:08:04.901747536 [rsc-session] Variant ID: 6465"
      )
    )
  )
})

test_that("non-leading timestamps remain", {
  expect_snapshot(
    stripConnectTimestamps(
      c(
        "this message has a timestamp 2024/04/24 13:08:04.901698921 within a line"
      )
    )
  )
})

test_that("messages without recognized timestamps are unmodified", {
  expect_snapshot(
    stripConnectTimestamps(
      c(
        "this message has no timestamp",
        "2024/04/24 13:08 this message timestamp has a different format"
      )
    )
  )
})

test_that("waitForTask", {
  skip_if_not_installed("webfakes")

  task_app <- webfakes::new_app()
  task_app$use(webfakes::mw_json())
  task_app$get("/v1/tasks/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = I(req$params$id),
        user_id = I(42),
        output = c(
          "2024/04/24 13:08:04.901698921 [rsc-session] Content GUID: 3bfbd98a-6d6d-41bd-a15f-cab52025742f",
          "2024/04/24 13:08:04.901734307 [rsc-session] Content ID: 43888",
          "2024/04/24 13:08:04.901742487 [rsc-session] Bundle ID: 94502",
          "2024/04/24 13:08:04.901747536 [rsc-session] Variant ID: 6465"
        ),
        result = NULL,
        finished = TRUE,
        code = 0,
        error = "",
        last = 4
      ), auto_unbox = TRUE
    )
  })
  app <- webfakes::new_app_process(task_app)
  service <- parseHttpUrl(app$url())

  authInfo <- list(
    secret = NULL,
    private_key = NULL,
    apiKey = "the-api-key",
    protocol = "https",
    certificate = NULL
  )
  client <- connectClient(service, authInfo)

  # task messages are logged when not quiet.
  expect_snapshot(invisible(client$waitForTask(101, quiet = FALSE)))
  # task messages are not logged when quiet.
  expect_snapshot(invisible(client$waitForTask(42, quiet = TRUE)))
})

# NOTE: These tests expect that you're already running connect; the tests
# will speak to that running connect process (if it can find it)
findConnect <- function() {
  connect <- Sys.which("connect")
  if (connect == "") {
    possibleLocs <- c(
      "~/git/connect/bin"
    )
    for (loc in possibleLocs) {
      if (file.exists(file.path(loc, "connect"))) {
        return(normalizePath(file.path(loc, "connect")))
      }
    }
    stop("Couldn't find an appropriate 'connect' binary")
  }
}

test_that("Users API", {

  skip_on_cran()
  skip_on_os("windows")
  skip("connect user test skipped.")

  ## rm db/*.db
  server <- serverInfo(findServer())

  connect <- connectClient(server$url, list())
  id <- paste(as.hexmode(sample(256, bytes) - 1), collapse = "")

  # add a user
  record <- userRecord(
    email = paste0("user", id, "@gmail.com"),
    username = paste0("user", id),
    first_name = "User",
    last_name = "Resu",
    password = paste0("password", id)
  )

  response <- connect$addUser(record)

  # check a couple main fields
  expect_equal(
    response[c("email", "first_name", "last_name", "username")],
    list(
      email = record$email,
      first_name = record$first_name,
      last_name = record$last_name,
      username = record$username
    )
  )

  # make sure we returned an empty password field (or no password field?)
  expect_true(response$password %in% "" || is.null(response$password))

  # generate a token
  accountId <- response$id
  token <- generateToken()

  tokenResponse <- connect$addToken(list(token = token$token,
                                         public_key = token$public_key,
                                         user_id = accountId))

  Sys.sleep(1)

  # open the URL in the browser
  utils::browseURL(tokenResponse$token_claim_url)

  # Sleep for a second so we can be sure that automatic auth happened
  Sys.sleep(2)

  # finally, create a fully authenticated client using the new token, and
  # keep trying to authenticate until we're successful
  connect <- connectClient(service = server$url, authInfo =
                             list(token = token$token,
                                  private_key = token$private_key))

  repeat {
    tryCatch({
      user <- connect$currentUser()
      break
    },
    error = function(e, ...) {
      # we expect this to return unauthorized until the token becomes active,
      # but bubble other errors
      if (length(grep("401 - Unauthorized", e$message)) == 0) {
        stop(e)
      }
      Sys.sleep(1)
    })
  }

  # Create and remove an example application

  ## Create it
  splineReticulator <- connect$createApplication("SplineReticulator")

  # Update the account id (since we have moved from unauthed user
  # to authed user)
  accountId <- splineReticulator$user_id

  ## Confirm it exists
  apps <- connect$listApplications(accountId)
  app <- apps[[which(sapply(apps, `[[`, "id") == splineReticulator$id)]]
  expect_true(app$name == "SplineReticulator")

  ## Upload a bundle
  pwd_splat <- strsplit(getwd(), "/")[[1]]
  if (pwd_splat[length(pwd_splat)] == "rsconnect") {
    path <- "tests/testthat/test-shinyApp/test-shinyApp.tar.gz"
  } else {
    path <- "test-shinyApp/test-shinyApp.tar.gz"
  }

  response <- connect$uploadApplication(
    splineReticulator$id,
    normalizePath(path = path)
  )

  ## Deploy an application
  appId <- response$app_id
  response <- connect$deployApplication(list(id = appId))
  id <- response$id

  ## Query the app for success / failure
  response <- connect$waitForTask(id)

  ## Terminate the application (does not remove it from store)
  connect$terminateApplication(appId)

  apps <- connect$listApplications(accountId)
  expect_false(response$id %in% sapply(apps, "[[", "id"))

})
