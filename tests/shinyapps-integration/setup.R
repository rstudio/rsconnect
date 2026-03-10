library(testthat)
library(rsconnect)

# Skip if shinyapps.io credentials are not available
shinyapps_name <- Sys.getenv("SHINYAPPS_NAME")
shinyapps_token <- Sys.getenv("SHINYAPPS_TOKEN")
shinyapps_secret <- Sys.getenv("SHINYAPPS_SECRET")

if (shinyapps_name == "" || shinyapps_token == "" || shinyapps_secret == "") {
  skip(
    "SHINYAPPS_NAME, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET must be set to run shinyapps.io integration tests."
  )
}

# Register the shinyapps.io account for testing
rsconnect::setAccountInfo(
  name = shinyapps_name,
  token = shinyapps_token,
  secret = shinyapps_secret
)

withr::defer(
  {
    removeAccount(shinyapps_name)
    # Clean up any rsconnect deployment artifacts
    files <- grep(
      paste0("rsconnect/shinyapps.io/", shinyapps_name),
      list.files(recursive = TRUE),
      value = TRUE
    )
    file.remove(files)
    dirs <- grep(
      paste0("rsconnect/shinyapps.io/", shinyapps_name),
      list.files(recursive = TRUE, include.dirs = TRUE),
      value = TRUE
    )
    file.remove(dirs)
    file.remove(dirname(dirs))
  },
  teardown_env()
)
