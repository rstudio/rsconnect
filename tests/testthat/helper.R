showDcf <- function(df) {
  write.dcf(df, stdout())
  invisible()
}

# last HTTP request made
httpLastRequest <- list()

# HTTP function which just saves the result for analysis
httpTestRecorder <- function(protocol,
                             host,
                             port,
                             method,
                             path,
                             headers,
                             contentType = NULL,
                             file = NULL,
                             certificate = NULL,
                             writer = NULL,
                             timeout = NULL) {
  httpLastRequest <<- list(
    protocol = protocol,
    host = host,
    port = port,
    method = method,
    path = path,
    headers = headers,
    contentType = contentType,
    file = file,
    certificate = certificate,
    writer = writer,
    timeout = timeout
  )

  list(status = 200, content = "", contentType = "plain/text")
}

local_http_recorder <- function(env = caller_env()) {
  withr::local_options(rsconnect.http = httpTestRecorder, .local_envir = env)
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
    writeLines(content, file.path(dir, name))
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

addTestAccount <- function(account = "ron", server = "example.com", userId = account) {
  registerAccount(server, account, userId, apiKey = "123")
  invisible()
}

addTestServer <- function(name = NULL, url = "https://example.com", certificate = NULL) {
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
addTestDeployment <- function(path,
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
                              metadata = list()) {
  saveDeployment(
    path,
    createDeploymentTarget(
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
