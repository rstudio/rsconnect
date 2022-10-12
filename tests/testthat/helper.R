
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

# Create directories as temporary replacements for HOME and R_USER_CONFIG_DIR
 # to avoid having tests overwrite the "official" configuration locations.
 #
 # test_that("some test", {
 #   havingFakeConfig({
 #     take_some_action(...)
 #     expect_something(...)
 #   })
 # })
havingFakeConfig <- function(expr) {
  temp_home_dir <- tempfile("home-")
  dir.create(temp_home_dir)
  temp_config_dir <- tempfile("config-")
  dir.create(temp_config_dir)
  
  home <- Sys.getenv("HOME")
  config <- Sys.getenv("R_USER_CONFIG_DIR")

  on.exit({
    # clean up temp folders
    unlink(temp_home_dir, recursive = TRUE)
    unlink(temp_config_dir, recursive = TRUE)

    # restore environment
    Sys.setenv(HOME = home)
    Sys.setenv(R_USER_CONFIG_DIR = config)
  }, add = TRUE)

  # temporarily change home to a temp folder so we don't litter the actual
  # home / config folder with test output
  Sys.setenv(HOME = temp_home_dir)
  # temporarily change the user config to use a temp folder so we don't litter
  # the actual config folder with test output
  Sys.setenv(R_USER_CONFIG_DIR = temp_config_dir)

  eval(expr)
}
