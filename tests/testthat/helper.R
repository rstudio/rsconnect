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
    "metadata",
    "quarto",
    "packages",
    "files",
    "users"
  )
  expect_in(names(manifest), known_fields)
}
