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
  if (!any(grepl("./bin/connect", system("ps -a", intern = TRUE), fixed = TRUE))) {
    stop("Couldn't find a running instance of connect")
  } else {
    TRUE
  }
}

test_that("RStudio Connect users API", {

  if (isConnectRunning()) {

    connect <- connectClient(list())
    id <- createUniqueId(16)

    # add a user
    record <- userRecord(
      id = 1L,
      email = paste0("user", id ,"@gmail.com"),
      username = paste0("user", id),
      first_name = "User",
      last_name = "Resu",
      password = paste0("password", id)
    )

    response <- connect$addUser(record)
    expect_equal(
      response[c("email", "first_name", "last_name")],
      list(
        email = record$email,
        first_name = record$first_name,
        last_name = record$last_name
      )
    )
    accountId <- response$id

    # now get a token by using the password we just made
    connect <- connectClient(list(username = record$username,
                                  password = record$password))

    token <- generateToken()
    tokenResponse <- connect$addToken(list(token = token$token,
                                           public_key = token$public_key))
    expect_equal(
      tokenResponse["token"],
      list(
        token = token$token
      )
    )
    # finally, create a fully authenticated client using the new token
    connect <- connectClient(list(token = token$token,
                                  private_key = token$private_key))

    # get that user's info again
    user <- connect$getUser(accountId)
    expect_equal(
      response[c("email", "first_name", "last_name")],
      user[c("email", "first_name", "last_name")]
    )

    # Create and remove an example application

    ## Create it
    splineReticulator <- connect$createApplication("SplineReticulator")

    ## Confirm it exists
    apps <- connect$listApplications(accountId)
    app <- apps[[which(sapply(apps, `[[`, "id") == splineReticulator$id)]]
    expect_true(app$name == "SplineReticulator")

    ## Upload a bundle
    response <- connect$uploadApplication(
      splineReticulator$id,
      normalizePath(path = "test-shinyApp/test-shinyApp.tar.gz")
    )

    ## Deploy an application
    appId <- response$app_id
    response <- connect$deployApplication(appId)
    id <- response$id

    ## Query the app for success / failure
    response <- connect$waitForTask(id)

    ## Delete an application
    connect$deleteApplication(response$id)
    apps <- connect$listApplications(accountId)
    expect_false(response$id %in% sapply(apps, "[[", "id"))

    ## Delete all applications
    ids <- sapply(apps, "[[", "id")
    lapply(ids, connect$deleteApplication)
    expect_identical(
      unclass(connect$listApplications(accountId)),
      list()
    )

  }

})
