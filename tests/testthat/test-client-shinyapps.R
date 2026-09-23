test_that("uploadBundle creates, uploads, marks ready, and returns the bundle", {
  skip_if_not_installed("webfakes")

  bundlePath <- withr::local_tempfile(fileext = ".tar.gz")
  writeLines("bundle contents", bundlePath)

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  # Step 1: register a pending bundle and return the presigned upload URL.
  app$post("/bundles", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = "bundle-1",
        presigned_url = paste0("http://", req$get_header("host"), "/upload"),
        presigned_checksum = "checksum-abc"
      ),
      auto_unbox = TRUE
    )
  })
  # Step 2: the presigned PUT. Status 200 means the upload worked.
  app$put("/upload", function(req, res) {
    res$set_status(200L)$send("")
  })
  # Step 3: mark the bundle ready. webfakes runs in a separate process, so the
  # server validates the status and returns 400 on a bad value; a clean 200
  # then proves the client sent status = "ready".
  app$post("/bundles/:id/status", function(req, res) {
    if (identical(req$json$status, "ready")) {
      res$set_status(200L)$send_json(list(), auto_unbox = TRUE)
    } else {
      res$set_status(400L)$send_json(
        list(error = "status must be ready"),
        auto_unbox = TRUE
      )
    }
  })
  # Step 4: return the updated bundle.
  app$get("/bundles/:id", function(req, res) {
    res$set_status(200L)$send_json(
      list(id = req$params$id, status = "ready"),
      auto_unbox = TRUE
    )
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(server = "shinyapps.io", name = "some-user")
  client <- shinyAppsClient(service, authInfo)

  bundle <- uploadBundle(
    client,
    list(application_id = "42"),
    bundlePath
  )

  expect_equal(bundle$id, "bundle-1")
  expect_equal(bundle$status, "ready")
})

test_that("uploadBundle stops when the presigned upload fails", {
  skip_if_not_installed("webfakes")

  bundlePath <- withr::local_tempfile(fileext = ".tar.gz")
  writeLines("bundle contents", bundlePath)

  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$post("/bundles", function(req, res) {
    res$set_status(200L)$send_json(
      list(
        id = "bundle-1",
        presigned_url = paste0("http://", req$get_header("host"), "/upload"),
        presigned_checksum = "checksum-abc"
      ),
      auto_unbox = TRUE
    )
  })
  # AWS rejects the upload.
  app$put("/upload", function(req, res) {
    res$set_status(403L)$send("")
  })
  proc <- webfakes::local_app_process(app)
  service <- parseHttpUrl(proc$url())

  authInfo <- list(server = "shinyapps.io", name = "some-user")
  client <- shinyAppsClient(service, authInfo)

  expect_error(
    uploadBundle(client, list(application_id = "42"), bundlePath),
    "Could not upload file"
  )
})
