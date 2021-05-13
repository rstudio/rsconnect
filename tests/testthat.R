library(testthat)
library(rsconnect)

# set temp config dir so the tests don't pollute it
temp_config_dir <- file.path(tempdir(), "rsconnect-test-config")
Sys.setenv(R_USER_CONFIG_DIR = temp_config_dir)

# perform tests
test_check("rsconnect")

# clean up temporary configuration
unlink(temp_config_dir, recursive = TRUE)
