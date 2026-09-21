library(testthat)
library(rsconnect)

# Configure the account() for testing, with cleanup
server <- Sys.getenv("CONNECT_SERVER")
apiKey <- Sys.getenv("CONNECT_API_KEY")
if (server == "" || apiKey == "") {
  stop(
    "CONNECT_SERVER and CONNECT_API_KEY must be set to run integration tests."
  )
}

# Generate a unique account name
account <- paste0("testing", strftime(Sys.time(), "%Y%m%d%H%M%S"))

# Apps deployed by the content-matrix cases in test-deploy.R get names
# starting with this prefix, so each fixture lands on a distinct app name
# even if this suite runs more than once against the same server.
run_prefix <- paste(sample(c(letters, LETTERS, 0:9), 5), collapse = "")

addServer(
  server,
  name = account,
  quiet = TRUE
)
connectApiUser(
  account = account,
  server = account,
  apiKey = apiKey,
  quiet = TRUE
)

withr::defer(
  {
    removeAccount(account)
    removeServer(account)
    # A deploy writes a record to <appDir>/rsconnect/<server>/<account>/.
    serverDirs <- grep(
      paste0("rsconnect/", account, "$"),
      list.dirs(".", recursive = TRUE),
      value = TRUE
    )
    unlink(serverDirs, recursive = TRUE)
    # Remove each rsconnect directory those records leave empty.
    for (dir in unique(dirname(serverDirs))) {
      if (length(list.files(dir, all.files = TRUE, no.. = TRUE)) == 0) {
        unlink(dir, recursive = TRUE)
      }
    }
    # Bundling content that has a _quarto.yml leaves a project cache behind.
    unlink(
      grep("\\.quarto$", list.dirs(".", recursive = TRUE), value = TRUE),
      recursive = TRUE
    )
  },
  teardown_env()
)
