context("connect")

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

isConnectRunning <- function() {
  any(grepl("./bin/connect", system2("ps", "-a", stdout = TRUE), fixed = TRUE))
}

test_that("Users API", {

  if (!isConnectRunning()) {
    cat("No running 'connect' instance detected -- tests skipped.")
    return()
  }

  ## rm db/*.db
  server <- getDefaultServer(local = TRUE)

  connect <- connectClient(server$url, list())
  id <- createUniqueId(16)

  # add a user
  record <- userRecord(
    email = paste0("user", id ,"@gmail.com"),
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
  response <- connect$deployApplication(appId)
  id <- response$id

  ## Query the app for success / failure
  response <- connect$waitForTask(id)

  ## Terminate the application (does not remove it from store)
  connect$terminateApplication(appId)

  apps <- connect$listApplications(accountId)
  expect_false(response$id %in% sapply(apps, "[[", "id"))

})
