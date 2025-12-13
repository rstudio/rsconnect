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
    # also clean up any artifacts left behind
    # do it twice, first to remove files, second to remove empty dirs
    file.remove(
      grep(
        "rsconnect/testing20",
        list.files(recursive = TRUE),
        value = TRUE
      )
    )
    dirs <- grep(
      "rsconnect/testing20",
      list.files(recursive = TRUE, include.dirs = TRUE),
      value = TRUE
    )
    # remove those dirs, then the rsconnect dir if empty
    file.remove(dirs)
    file.remove(dirname(dirs))
  },
  teardown_env()
)
