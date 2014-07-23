context("connect")

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

connectRunning <- function() {
  any(grepl("./bin/connect", system("ps -a", intern = TRUE), fixed = TRUE))
}

checkConnectRunning <- function() {
  if (!connectRunning()) {
    stop("'connect' is not currently running; cannot run tests")
  }
}

dummyConnectClient <- function() {
  connectClient(list(token = "token", secret = "secret"))
}

test_that("RStudio Connect users API", {

  checkConnectRunning()

  connect <- dummyConnectClient()

  # add a user
  record <- userRecord(
    id = 1L,
    email = "user@gmail.com",
    first_name = "User",
    last_name = "Resu"
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

})

test_that("RStudio Connect applications API", {

  checkConnectRunning()
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
    normalizePath(path = "internal/test-shinyApp.tar.gz")
  )

  ## Delete an application
  connect$deleteApplication(app$id)
  apps <- connect$listApplications()
  expect_false(app$id %in% sapply(apps, "[[", "id"))

})
