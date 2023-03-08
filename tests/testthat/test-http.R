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

local({
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

withr::defer({
  # Clean up temp files
  unlink(output)
  unlink(input)
  unlink(datafile)

  # Kill the R process hosting the background http server
  server$kill()
})
