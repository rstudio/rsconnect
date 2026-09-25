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

# Existing PCC content with a NULL current_revision.
pcc_existing_content_null_revision <- list(
  id = "content-abc",
  url = "https://connect.posit.cloud/myaccount/content/content-abc",
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

# A PCC client whose deleteContent() passes the content id to `deleted`.
local_mock_pcc_delete_client <- function(deleted, env = caller_env()) {
  local_mocked_bindings(
    clientForAccount = function(...) {
      fake_client(
        "connectCloudClient",
        getContent = function(contentId) {
          list(id = contentId, title = "My App")
        },
        deleteContent = function(contentId) {
          deleted(contentId)
          invisible(TRUE)
        }
      )
    },
    .env = env
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
