library(testthat)
library(rsconnect)

run_tests <- function() {
  # set temp config dir so the tests don't pollute it
  temp_config_dir <- file.path(tempdir(), "rsconnect-test-config")
  Sys.setenv(R_USER_CONFIG_DIR = temp_config_dir)

  # clean up temp dir after tests are run
  on.exit({
    unlink(temp_config_dir, recursive = TRUE)
  }, add = TRUE)

  # perform tests
  test_check("rsconnect")
}

run_tests()
