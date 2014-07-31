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

dummyConnectClient <- function() {
  connectClient(list(token = "token", secret = "secret"))
}

test_that("RStudio Connect users API", {

  if (isConnectRunning()) {

    connect <- dummyConnectClient()

    # add a user
    record <- userRecord(
      id = 1L,
      email = "user@gmail.com",
      first_name = "User",
      last_name = "Resu",
      password = "some really hard to break password"
    )

    response <- connect$addUser(record)
    expect_equal(
      response[c("email", "first_name", "last_name")],
      list(
        email = "user@gmail.com",
        first_name = "User",
        last_name = "Resu"
      )
    )

    # get that user's info again
    user <- connect$getUser(response$id)
    expect_equal(
      response[c("email", "first_name", "last_name")],
      user[c("email", "first_name", "last_name")]
    )

  }

})

test_that("RStudio Connect applications + tasks API", {

  if (isConnectRunning()) {

    connect <- dummyConnectClient()

    # Create and remove an example application

    ## Create it
    splineReticulator <- connect$createApplication("SplineReticulator")

    ## Confirm it exists
    apps <- connect$listApplications()
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
    apps <- connect$listApplications()
    expect_false(response$id %in% sapply(apps, "[[", "id"))

    ## Delete all applications
    ids <- sapply(apps, "[[", "id")
    lapply(ids, connect$deleteApplication)
    expect_identical(
      unclass(connect$listApplications()),
      list()
    )

  }

})
