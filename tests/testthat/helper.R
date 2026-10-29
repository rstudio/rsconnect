showDcf <- function(df) {
  write.dcf(df, stdout())
  invisible()
}

# Create and use a directory as temporary replacement for R_USER_CONFIG_DIR to
# avoid having tests overwrite the "official" configuration locations.
local_temp_config <- function(env = caller_env()) {
  path <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(R_USER_CONFIG_DIR = path, .local_envir = env)
}

local_temp_app <- function(files = list(), env = caller_env()) {
  dir <- withr::local_tempdir(.local_envir = env)

  for (name in names(files)) {
    content <- files[[name]]
    hier <- dirname(name)
    if (!hier == ".") {
      dir.create(file.path(dir, hier), recursive = TRUE)
    }
    if (length(content) > 0) {
      writeLines(content, file.path(dir, name))
    } else {
      file.create(file.path(dir, name))
    }
  }

  dir
}


local_shiny_bundle <- function(appName, appDir, appPrimaryDoc, python = NULL) {
  appFiles <- bundleFiles(appDir)
  appMetadata <- appMetadata(appDir, appFiles, appPrimaryDoc = appPrimaryDoc)

  tarfile <- bundleApp(
    appName,
    appDir,
    appFiles = appFiles,
    appMetadata = appMetadata,
    pythonConfig = pythonConfigurator(python),
    quiet = TRUE
  )
  bundleTempDir <- tempfile()
  utils::untar(tarfile, exdir = bundleTempDir)
  unlink(tarfile)

  defer(unlink(bundleTempDir, recursive = TRUE), env = caller_env())
  bundleTempDir
}

# Node.js content that appMetadata() accepts. A package-lock.json is required.
local_nodejs_app <- function(env = caller_env()) {
  local_temp_app(
    list(
      "index.js" = "",
      "package.json" = '{"name": "app", "main": "index.js"}',
      "package-lock.json" = '{"name": "app", "lockfileVersion": 3}'
    ),
    env = env
  )
}

# A classed stand-in for connectClient()/shinyAppsClient()/
# connectCloudClient(), for tests that only need a couple of methods.
# Attaches the class vector so generics dispatch the same way they would on
# a real client, and keeps any closures passed in `...` as fields for
# methods that are not yet generics.
fake_client <- function(class, ...) {
  stopifnot(class %in% client_classes())
  structure(list(...), class = c(class, "rsconnectClient"))
}

client_classes <- function() {
  c("connectClient", "shinyAppsClient", "connectCloudClient")
}

client_generics <- function() {
  generics <- lapply(client_classes(), function(class) {
    attr(utils::.S3methods(class = class), "info")$generic
  })
  unique(unlist(generics))
}

has_client_method <- function(generic, class) {
  method <- getS3method(generic, class, optional = TRUE)
  if (is.null(method)) {
    method <- getS3method(generic, "rsconnectClient", optional = TRUE)
  }
  !is.null(method)
}

expect_client_method <- function(generic, class) {
  testthat::expect(
    has_client_method(generic, class),
    sprintf("expected a method for `%s.%s`", generic, class)
  )
}


# Servers and accounts ----------------------------------------------------

addTestAccount <- function(
  account = "ron",
  server = "example.com",
  userId = account
) {
  registerAccount(server, account, userId, apiKey = "123")
  invisible()
}

addTestServer <- function(
  name = NULL,
  url = "https://example.com",
  certificate = NULL
) {
  if (is.null(name)) {
    serverUrl <- parseHttpUrl(url)
    name <- serverUrl$host
  }

  registerServer(
    url = url,
    name = name,
    certificate = certificate
  )
  invisible()
}
addTestDeployment <- function(
  path,
  appName = "test",
  appTitle = "",
  appId = "123",
  account = "ron",
  envVars = NULL,
  username = account,
  server = "example.com",
  url = paste0("https://", server, "/", username, "/", appId),
  hostUrl = NULL,
  version = deploymentRecordVersion,
  metadata = list()
) {
  saveDeployment(
    path,
    createDeployment(
      appName = appName,
      appTitle = appTitle,
      appId = appId,
      envVars = envVars,
      account = account,
      username = username,
      server = server,
      version = version
    ),
    application = list(id = appId, url = url),
    hostUrl = hostUrl,
    metadata = metadata,
    addToHistory = FALSE
  )
}

local_mocked_account_info <- function(env = caller_env()) {
  local_mocked_bindings(
    accountInfo = function(name = NULL, server = NULL) {
      list(name = name, server = server)
    },
    .env = env
  )
}

# Posit Connect Cloud deploy fixtures -------------------------------------

# Existing PCC content with a NULL current_revision. Connect Cloud content has
# no top-level `url`; only a revision has one.
pcc_existing_content_null_revision <- list(
  id = "content-abc",
  current_revision = NULL,
  next_revision = list(
    id = "rev-new",
    source_bundle_upload_url = "https://upload.example.com/bundle"
  )
)

# Existing PCC content with a current_revision.
pcc_existing_content_with_revision <- utils::modifyList(
  pcc_existing_content_null_revision,
  list(current_revision = list(id = "rev-old"))
)

# An activateContent() method for PCC that reports a successful deploy.
pcc_activate_success <- function(client, application, bundle, quiet) {
  list(
    succeeded = TRUE,
    url = "https://connect.posit.cloud/myaccount/content/content-abc",
    error = NULL
  )
}

# Set up a PCC server, account, and (optionally) a re-deploy record.
local_pcc_deploy_env <- function(
  appDir,
  appId = "content-abc",
  env = parent.frame()
) {
  local_temp_config(env = env)
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")
  if (!is.null(appId)) {
    addTestDeployment(
      appDir,
      appName = "myapp",
      appId = appId,
      account = "myaccount",
      server = "connect.posit.cloud"
    )
  }
}

# A PCC client whose content delete passes the content id to `deleted`.
local_mock_pcc_delete_client <- function(deleted, env = caller_env()) {
  local_mocked_bindings(
    clientForAccount = function(...) fake_client("connectCloudClient"),
    connectCloudGetContent = function(client, contentId) {
      list(id = contentId, title = "My App")
    },
    connectCloudDeleteContent = function(client, contentId) {
      deleted(contentId)
      invisible(TRUE)
    },
    .env = env
  )
}

# calls createContent() for a Connect Cloud client with a small deployment.
create_cloud_content <- function(client, envVars = NULL, appVisibility = NULL) {
  createContent(
    client,
    deployment = list(name = "my-app", title = "My App", envVars = envVars),
    accountDetails = list(accountId = "acct-1"),
    appMetadata = list(appMode = "shiny", appPrimaryDoc = "app.R"),
    appVisibility = appVisibility
  )
}

# Calls prepareContent() for existing Connect Cloud content with a small
# deployment.
update_cloud_content <- function(
  client,
  envVars = NULL,
  appVisibility = NULL,
  upload = FALSE,
  application = list(id = "content-abc")
) {
  prepareContent(
    client,
    application = application,
    deployment = list(envVars = envVars),
    appMetadata = list(appMode = "shiny", appPrimaryDoc = "app.R"),
    appVisibility = appVisibility,
    isNewContent = FALSE,
    upload = upload,
    quiet = TRUE
  )
}

# adding a top-level manifest field is allowed,
# but requires coordination with the hosted team
# to avoid upstream issues. In particular,
# shinyapps.io enforces a strict manifest schema
# that will need to be updated to accomodate the change
#
# this relates primarily to adding a new always present
# top level field from within writeManifest
expect_known_manifest_fields <- function(manifest) {
  known_fields <- c(
    "version",
    "environment",
    "platform",
    "locale",
    "python",
    "nodejs",
    "metadata",
    "quarto",
    "packages",
    "files",
    "users"
  )
  expect_in(names(manifest), known_fields)
}

# Connect Cloud client credentials ---------------------------------------

# Builds a fake Connect Cloud client whose requests return the supplied
# accounts as one page. Used to drive connectCloudClientCredentials in tests
# without hitting the network.
fakeCloudClient <- function(accounts) {
  fake_client(
    "connectCloudClient",
    withTokenRefreshRetry = function(request_fn, path, ...) {
      list(data = accounts, total = length(accounts))
    }
  )
}

# Mocks the OAuth exchange that connectCloudClientCredentials performs before
# touching the API.
mockClientCredentialsAuth <- function(
  accessToken = "access-token",
  refreshToken = NULL
) {
  function() {
    list(
      exchangeClientCredentials = function(clientId, clientSecret) {
        list(access_token = accessToken, refresh_token = refreshToken)
      }
    )
  }
}

# Quarto -----------------------------------------------------------------

fakeQuartoMetadata <- function(version, engines) {
  # See quarto-r/R/publish.R lines 396 and 113.
  metadata <- list()
  metadata$quarto_version <- version
  metadata$quarto_engines <- I(engines)
  return(metadata)
}

# Some versions of Quarto show Quarto stack traces with source references when
# inspect fails.
#
# Only preserve:
#
#   Error in `quartoInspect()`:
#   ! Failed to run `quarto inspect` against your content:
#   ERROR: Unsupported project type unsupported
strip_quarto_trace <- function(lines) {
  head(lines, 3)
}

# Deployment targets -----------------------------------------------------

confirm_existing_app_used <- function(server) {
  local_temp_config()
  addTestServer()
  addTestAccount("ron", server = server)
  local_mocked_bindings(
    getAppByName = function(...) {
      data.frame(
        name = "my_app",
        id = 123,
        url = "http://example.com/test",
        stringsAsFactors = FALSE
      )
    },
    shouldUpdateApp = function(...) TRUE
  )

  app_dir <- withr::local_tempdir()
  target <- findDeploymentTarget(app_dir, appName = "my_app", server = server)
  deployment <- target$deployment
  expect_equal(deployment$appId, 123)
}

confirm_existing_app_not_used <- function(server) {
  local_temp_config()
  addTestServer()
  addTestAccount("ron", server = server)
  local_mocked_bindings(
    getAppByName = function(...) {
      data.frame(
        name = "my_app",
        id = 123,
        url = "http://example.com/test",
        stringsAsFactors = FALSE
      )
    },
    shouldUpdateApp = function(...) FALSE
  )

  app_dir <- withr::local_tempdir()
  target <- findDeploymentTarget(app_dir, appName = "my_app", server = server)
  deployment <- target$deployment
  expect_equal(deployment$name, "my_app-1")
  expect_equal(deployment$appId, NULL)
}

# Identity federation ----------------------------------------------------

# Helper to create a clean API key cache for tests
local_api_key_cache <- function(env = caller_env()) {
  nms <- env_names(apiKeyCache)
  zaps <- rep_named(nms, list(zap()))

  old <- env_bind(apiKeyCache, !!!zaps)
  withr::defer(env_bind(apiKeyCache, !!!old), envir = env)
}

# Migration to Connect Cloud ---------------------------------------------

# Helper: write a minimal fixture DCF for a shinyapps.io deployment.
write_shinyapps_dcf <- function(
  appDir,
  appName = "myapp",
  account = "myaccount"
) {
  dcfDir <- file.path(appDir, "rsconnect", "shinyapps.io", account)
  dir.create(dcfDir, recursive = TRUE)
  dcfPath <- file.path(dcfDir, paste0(appName, ".dcf"))
  write.dcf(
    list(
      name = appName,
      title = "My App",
      username = account,
      account = account,
      server = "shinyapps.io",
      hostUrl = "https://api.shinyapps.io/v1",
      appId = "42",
      bundleId = "7",
      url = paste0("https://", account, ".shinyapps.io/", appName),
      version = "1"
    ),
    dcfPath,
    width = 4096
  )
  dcfPath
}

# An accounts() frame with both a shinyapps.io and a Connect Cloud account.
shinyapps_and_cc_accounts <- data.frame(
  name = c("myaccount", "cc-account"),
  server = c("shinyapps.io", "connect.posit.cloud"),
  stringsAsFactors = FALSE
)

# Helper: install the Connect Cloud mocks migrateToConnectCloud() relies on.
# Every knob has a sensible default; tests override only what they exercise.
#   - accounts:       the accounts() frame (defaults to a single CC account)
#   - title:          content title returned by getContent()
#   - account_id:     owning account id returned by getContent()
#   - owned_accounts: the accounts getAccounts() reports a role on
#   - get_content:    override the whole getContent() function (e.g. to error)
#   - client:         override the whole clientForAccount() function
local_migrate_mocks <- function(
  accounts = data.frame(
    name = "cc-account",
    server = "connect.posit.cloud",
    stringsAsFactors = FALSE
  ),
  title = "My App",
  account_id = "acct-1",
  owned_accounts = list(list(id = "acct-1", name = "cc-account")),
  get_content = NULL,
  client = NULL,
  .env = parent.frame()
) {
  get_content <- get_content %||%
    function(id) {
      list(id = id, title = title, account_id = account_id, state = "active")
    }
  local_mocked_bindings(
    connectCloudGetContent = function(client, contentId) get_content(contentId),
    connectCloudGetAccounts = function(client) list(data = owned_accounts),
    .env = .env
  )
  client <- client %||%
    function(...) {
      fake_client("connectCloudClient")
    }
  local_mocked_bindings(
    accounts = function(...) accounts,
    findAccountInfo = function(...) {
      list(
        name = "cc-account",
        server = "connect.posit.cloud",
        accessToken = "tok"
      )
    },
    clientForAccount = client,
    .package = "rsconnect",
    .env = .env
  )
}

# Manifests --------------------------------------------------------------

makeManifest <- function(appDir, appPrimaryDoc = NULL, ...) {
  writeManifest(appDir, appPrimaryDoc = appPrimaryDoc, ..., quiet = TRUE)
  manifestFile <- file.path(appDir, "manifest.json")
  data <- readLines(manifestFile, warn = FALSE, encoding = "UTF-8")
  manifestJson <- jsonlite::fromJSON(data)
  unlink(manifestFile)
  manifestJson
}
