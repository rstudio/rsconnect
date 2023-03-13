
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
    writeLines(content, file.path(dir, name))
  }

  dir
}

# Servers and accounts ----------------------------------------------------

addTestAccount <- function(account, server = "example.com", userId = account) {
  registerUserToken(server, account, userId, "", "")
}

addTestServer <- function(name = NULL, url = "http://example.com", certificate = NULL) {

  if (is.null(name)) {
    serverUrl <- parseHttpUrl(url)
    name <- serverUrl$host
  }

  addServer(
    url = url,
    name = name,
    certificate = certificate,
    validate = FALSE,
    quiet = TRUE
  )
}
