library(testthat)
library(rsconnect)

# record whether the configuration directory exists
configDir <- rsconnect:::rsconnectConfigDir()
configDirExists <- file.exists(configDir)

test_check("rsconnect")

# if the configuration directory did not exist before running the tests, clean it up.
if (!configDirExists) {
  unlink(configDir, recursive = TRUE)
}
