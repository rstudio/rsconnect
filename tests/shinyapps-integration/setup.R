library(testthat)
library(rsconnect)

# Skip if shinyapps.io credentials are not available
shinyapps_name <- Sys.getenv("SHINYAPPS_NAME")
shinyapps_token <- Sys.getenv("SHINYAPPS_TOKEN")
shinyapps_secret <- Sys.getenv("SHINYAPPS_SECRET")

if (shinyapps_name == "" || shinyapps_token == "" || shinyapps_secret == "") {
  stop(
    "SHINYAPPS_NAME, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET must be set to run shinyapps.io integration tests."
  )
}
original_repos <- getOption("repos")
if ("RSPM" %in% names(original_repos) && !"CRAN" %in% names(original_repos)) {
  cran_repos <- original_repos
  names(cran_repos)[names(cran_repos) == "RSPM"] <- "CRAN"
  options(repos = cran_repos)
  withr::defer(options(repos = original_repos), teardown_env())
}
# Register the shinyapps.io account for testing
rsconnect::setAccountInfo(
  name = shinyapps_name,
  token = shinyapps_token,
  secret = shinyapps_secret
)

# apps from this run will have names starting with this prefix. we use this on
# cleanup to know which apps to purge from the test account.
run_prefix <- paste(sample(c(letters, LETTERS, 0:9), 5), collapse = "")

withr::defer(
  {
    tryCatch(
      {
        apps <- applications(account = shinyapps_name)
        to_purge <- apps[grepl(paste0("^", run_prefix), apps$name), "name"]
        # purge applications from the current run
        lapply(to_purge, purgeApp, account = shinyapps_name)
      },
      error = function(e) {
        warning(
          "Unable to clean up test applications from shinyapps.io account"
        )
      }
    )
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
