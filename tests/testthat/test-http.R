context("http")

# The list of HTTP transports to check
transports <- c("libcurl", "rcurl", "curl", "internal")

# Temporary file to cache the output (request) to HTTP methods
output <- tempfile(fileext = ".rds")

# Temporary file to cache the input (response) to HTTP methods
input <- tempfile(fileext = ".rds")

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
  # Create a new server and run it in a background R process
  server <<- callr::r_bg(func = function(output, input, port) {
    httpuv::runServer(host = "127.0.0.1", port = port,
      app = list(
        call = function(req) {
          # save the request to a file for examination
          saveRDS(object = req, file = output)

          # parse and return the input file
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

  # Kill the R process hosting the background http server
  server$kill()
})

test_that("simple http GET works", {
  for(transport in transports) {
    options("rsconnect.http" = transport)
    saveRDS(file = input, object = list(
      status = 200L,
      headers = list(
        "Content-Type" = "text/plain"
      ),
      body = "GET successful"
    ))
    GET(service = service,
        authInfo = NULL,
        query = NULL,
        path = "test")
    request <- readRDS(output)
    expect_equal(request$REQUEST_METHOD, "GET")
    expect_equal(request$PATH_INFO, "/test")
  }
})

test_that("posting JSON works", {
  for(transport in transports) {
    options("rsconnect.http" = transport)
    saveRDS(file = input, object = list(
      status = 200L,
      headers = list(
        "Content-Type" = "text/plain"
      ),
      body = "POST successful"
    ))

    body <- list(a = 1, b = 2, c = 3)

    POST_JSON(service = service,
        authInfo = NULL,
        json = body,
        query = NULL,
        path = "test")
    request <- readRDS(output)

    expect_equal(request$REQUEST_METHOD, "POST")

    # TODO: validate request
    # req <- request[["rook.input"]]$read_lines()
    # expect_equal(req, toJSON(body, pretty = TRUE))
  }
})
