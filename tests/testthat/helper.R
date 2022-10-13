
# last HTTP request made
httpLastRequest <- list()

# HTTP function which just saves the result for analysis
httpTestRecorder <- function(protocol, host, port, method, path, headers,
                             contentType = NULL, file = NULL, certificate = NULL,
                             writer = NULL, timeout = NULL)
{
  httpLastRequest <<- list(
    protocol = protocol,
    host = host,
    port = port,
    method = method,
    path = path,
    headers = headers,
    contentType = contentType,
    file = file,
    certificate = certificate,
    writer = writer,
    timeout = timeout
  )
}

# Create and use a directory as temporary replacement for R_USER_CONFIG_DIR to
# avoid having tests overwrite the "official" configuration locations.
#
# test_that("some test", {
#   havingFakeConfig({
#     take_some_action(...)
#     expect_something(...)
#   })
# })
#
# This function is similar in purpose to the run_tests function in
# ../testthat.R, but uses per-test replacement location rather than a single
# replacement location across all tests.
#
# Use this function when test state needs to be isolated from other tests.
havingFakeConfig <- function(expr) {
  # Calculate and create temporary state location
  temp_config_dir <- tempfile("config-")
  dir.create(temp_config_dir)

  # Preserve incoming state
  config <- Sys.getenv("R_USER_CONFIG_DIR")


  # Restore incoming state and remove temporary location on exit.
  on.exit({
    Sys.setenv(R_USER_CONFIG_DIR = config)

    unlink(temp_config_dir, recursive = TRUE)
  }, add = TRUE)

  # Use temporary state while testing to avoid manipulating the actual state.
  Sys.setenv(R_USER_CONFIG_DIR = temp_config_dir)

  eval(expr)
}
