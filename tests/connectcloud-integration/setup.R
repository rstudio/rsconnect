library(testthat)
library(rsconnect)

# Required env vars:
#   CONNECT_CLOUD_CLIENT_ID     — OAuth client ID for a service account
#   CONNECT_CLOUD_CLIENT_SECRET — paired secret
#   CONNECT_CLOUD_ACCOUNT       — Connect Cloud account name (e.g. "myorg")
#
# One-time setup: obtain client credentials at
# https://connect.posit.cloud/identity/credentials

cc_client_id <- Sys.getenv("CONNECT_CLOUD_CLIENT_ID")
cc_client_secret <- Sys.getenv("CONNECT_CLOUD_CLIENT_SECRET")
cc_account <- Sys.getenv("CONNECT_CLOUD_ACCOUNT")

if (cc_client_id == "" || cc_client_secret == "" || cc_account == "") {
  stop(
    "CONNECT_CLOUD_CLIENT_ID, CONNECT_CLOUD_CLIENT_SECRET, and CONNECT_CLOUD_ACCOUNT ",
    "must be set to run Connect Cloud integration tests."
  )
}

# Use a unique local alias so we never clobber a pre-existing account entry
# with the same name in the developer's local config (mirrors integration/setup.R).
cc_local_name <- paste0("testing-cc-", strftime(Sys.time(), "%Y%m%d%H%M%S"))

rsconnect::connectCloudClientCredentials(
  clientId = cc_client_id,
  clientSecret = cc_client_secret,
  accountName = cc_account,
  name = cc_local_name,
  quiet = TRUE
)

withr::defer(
  removeAccount(cc_local_name, server = "connect.posit.cloud"),
  teardown_env()
)
