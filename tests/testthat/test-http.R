context("http")

# Test HTTP server doesn't work on Solaris
skip_on_os("solaris")

skip_if_not_installed("callr")
skip_if_not_installed("httpuv")

# Temporary file to cache the output (request) to HTTP methods
output <- tempfile(fileext = ".rds")

# Temporary file to cache the input (response) to HTTP methods
input <- tempfile(fileext = ".rds")

# Temporary file to send test posting file to server
datafile <- tempfile("rsconnect-tmp")

# Configure the test service we'll be connecting to
port <- 4072L
service <- list(
  protocol = "http",
  host = "localhost",
  port = port,
  path = "/")

# Holds the process object referring to the background HTTP server
server <- NULL

setup({
  # Ensure temp directory exists
  if (!dir.exists(dirname(input)))
    dir.create(dirname(input), recursive = TRUE)

  # Seed input with sample data
  saveRDS(file = input, object = list(
    status = 200L,
    headers = list(
      "Content-Type" = "text/plain"
    ),
    body = "Request successful"
  ))

  # Create a new server and run it in a background R process
  server <<- callr::r_bg(func = function(output, input, port) {
    httpuv::runServer(host = "127.0.0.1", port = port,
      app = list(
        call = function(req) {
          # Eagerly fetch the body
          body <- paste(req[["rook.input"]]$read_lines(), collapse = "\n")
          req[["body"]] <- body

          # Save the request to a file for examination
          saveRDS(object = req, file = output)

          # Parse and return the input file
          readRDS(file = input)
        }))
      },
    args = list(output = output, input = input, port = port))

  # Sleep a couple of seconds to give the server time to start
  Sys.sleep(2)
})

teardown({
  # Clean up temp files
  unlink(output)
  unlink(input)
  unlink(datafile)

  # Kill the R process hosting the background http server
  server$kill()
})

test_http_GET <- function(transport) {
  # Set the transport for this instance of the test
  old <- options("rsconnect.http" = transport)
  on.exit(options(old), add = TRUE)

  # Save the response the server will return
  saveRDS(file = input, object = list(
    status = 200L,
    headers = list(
      "Content-Type" = "text/plain"
    ),
    body = "GET successful"
  ))

  # Perform the request
  GET(service = service,
      authInfo = NULL,
      query = NULL,
      path = "test")

  # Validate that we performed a GET on the requested path
  request <- readRDS(output)
  expect_equal(request$REQUEST_METHOD, "GET")
  expect_equal(request$PATH_INFO, "/test")
}

test_that("simple http GET works (libcurl)", {
  test_http_GET("libcurl")
})

test_that("simple http GET works (internal)", {
  test_http_GET("internal")
})

test_that("simple http GET works (RCurl)", {
  skip_if_not_installed("RCurl")
  test_http_GET("rcurl")
})

test_that("simple http GET works (curl)", {
  skip_if(Sys.which("curl") == "")
  test_http_GET("curl")
})

test_http_POST_JSON <- function(transport) {
  # Set the transport for this instance of the test
  old <- options("rsconnect.http" = transport)
  on.exit(options(old), add = TRUE)

  saveRDS(file = input, object = list(
    status = 200L,
    headers = list(
      "Content-Type" = "text/plain"
    ),
    body = "POST successful"
  ))

  # Perform the request
  body <- list(a = 1, b = 2, c = 3)
  POST_JSON(service = service,
            authInfo = NULL,
            json = body,
            query = NULL,
            path = "test")

  # Validate HTTP method
  request <- readRDS(output)
  expect_equal(request$REQUEST_METHOD, "POST")

  # Validate body contents
  expect(request$body == toJSON(body, pretty = TRUE),
         failure_message =
           paste0("Unexpected request body '", request$body, "', with transport ", transport))
}

test_that("posting JSON works (libcurl)", {
  test_http_POST_JSON("libcurl")
})

test_that("posting JSON works (internal)", {
  test_http_POST_JSON("internal")
})

test_that("posting JSON works (RCurl)", {
  skip_if_not_installed("RCurl")
  test_http_POST_JSON("rcurl")
})

test_that("posting JSON works (libcurl)", {
  skip_if(Sys.which("curl") == "")
  test_http_POST_JSON("curl")
})

test_http_POST_empty <- function(transport) {
  # Set the transport for this instance of the test
  old <- options("rsconnect.http" = transport)
  on.exit(options(old), add = TRUE)

  # Save the response the server will return
  saveRDS(file = input, object = list(
    status = 200L,
    headers = list(
      "Content-Type" = "text/plain"
    ),
    body = "POST successful"
  ))

  # Perform the request
  POST(service = service,
       authInfo = NULL,
       path = "test",
       file = NULL,
       content = NULL)

  # Validate HTTP method
  request <- readRDS(output)
  expect_equal(request$REQUEST_METHOD, "POST")

  # Validate body contents
  expect(request$body == "",
         failure_message =
           paste0("Unexpected request body '", request$body, "', with transport ", transport))
}

test_that("posting with no data works (libcurl)", {
  test_http_POST_empty("libcurl")
})

test_that("posting with no data works (internal)", {
  test_http_POST_empty("internal")
})

test_that("posting with no data works (RCurl)", {
  skip_if_not_installed("RCurl")
  test_http_POST_empty("rcurl")
})

test_that("posting with no data works (curl)", {
  skip_if(Sys.which("curl") == "")
  test_http_POST_empty("curl")
})

test_http_POST_file <- function(transport) {
  # Set the transport for this instance of the test
  old <- options("rsconnect.http" = transport)
  on.exit(options(old), add = TRUE)

  # Save the response the server will return
  saveRDS(file = input, object = list(
    status = 200L,
    headers = list(
      "Content-Type" = "text/plain"
    ),
    body = "POST successful"
  ))

  # Perform the request
  write(c("1", "2", "3"), datafile)
  POST(service = service,
       authInfo = NULL,
       path = "test",
       contentType = "text/plain",
       file = datafile,
       content = NULL)

  # Validate HTTP method
  request <- readRDS(output)
  expect_equal(request$REQUEST_METHOD, "POST")

  # Validate body contents
  expect(request$body == "1\n2\n3",
         failure_message =
           paste0("Unexpected request body '", request$body, "', with transport ", transport))
}

test_that("posting file works (libcurl)", {
  test_http_POST_file("libcurl")
})

test_that("posting file works (internal)", {
  test_http_POST_file("internal")
})

test_that("posting file works (RCurl)", {
  skip_if_not_installed("RCurl")
  test_http_POST_file("rcurl")
})

test_that("posting file works (curl)", {
  skip_if(Sys.which("curl") == "")
  test_http_POST_file("curl")
})

test_http_api_headers <- function(transport) {
  # Set the transport for this instance of the test
  old <- options("rsconnect.http" = transport)
  on.exit(options(old), add = TRUE)

  # Save the response the server will return
  saveRDS(file = input, object = list(
    status = 200L,
    headers = list(
      "Content-Type" = "text/plain"
    ),
    body = "GET successful"
  ))
  apiKey <- "abc123"
  # Perform the request
  GET(service = service,
      authInfo = list(apiKey = apiKey),
      query = NULL,
      path = "test")
  # Validate that we performed a GET on the requested path
  request <- readRDS(output)

  # Validate header contents
  expect(request$HEADERS["authorization"] == paste("Key", apiKey),
         failure_message =
           paste0("Correct api key request header missing in '",
                  request$HEADERS, "', with transport ", transport))
}

test_that("api key authinfo sets headers (libcurl)", {
  test_http_api_headers("libcurl")
})

test_that("api key authinfo sets headers (internal)", {
  test_http_api_headers("internal")
})

test_that("api key authinfo sets headers (RCurl)", {
  skip_if_not_installed("RCurl")
  test_http_api_headers("rcurl")
})

test_that("api key authinfo sets headers (curl)", {
  skip_if(Sys.which("curl") == "")
  test_http_api_headers("curl")
})
