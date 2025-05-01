#' Server metadata
#'
#' `servers()` lists all known servers; `serverInfo()` gets metadata about
#' a specific server. Cloud servers `shinyapps.io` and `posit.cloud` are always
#' automatically registered and available.
#'
#' @param name Server name. If omitted, you'll be prompted to pick a server.
#' @param local Return only local servers? (i.e. not automatically registered
#'   cloud servers)
#' @return
#' `servers()` returns a data frame with registered server names and URLs.
#' `serverInfo()` returns a list with details for a particular server.
#' @export
#' @examples
#' # List all registered servers
#' servers()
#'
#' # Get information about a server
#' serverInfo("posit.cloud")
servers <- function(local = FALSE) {
  servers <- serverNames(local)

  info <- lapply(servers, serverInfo)
  info <- lapply(info, as.data.frame, stringsAsFactors = FALSE)

  out <- rbind_fill(info, c("name", "url", "certificate"))
  out$certificate <- secret(out$certificate)
  out
}

#' @rdname servers
#' @export
serverInfo <- function(name = NULL) {
  name <- findServer(name, local = FALSE)

  if (isPositCloudServer(name)) {
    info <- cloudServerInfo(name, "https://api.posit.cloud/v1")
  } else if (isShinyappsServer(name)) {
    info <- cloudServerInfo(name, "https://api.shinyapps.io/v1")
  } else {
    configFile <- serverConfigFile(name)
    serverDcf <- read.dcf(serverConfigFile(name), all = TRUE)
    info <- as.list(serverDcf)
  }

  info$certificate <- secret(info$certificate)
  info
}

serverNames <- function(local = FALSE) {
  names <- gsub("\\.dcf$", "", basename(serverConfigFiles()))
  if (!local) {
    names <- c(names, "shinyapps.io", "posit.cloud")

    if (nrow(accounts(server = "rstudio.cloud")) > 0) {
      names <- c(names, "rstudio.cloud")
    }
  }

  names
}

isShinyappsServer <- function(server) {
  identical(server, "shinyapps.io")
}

isPositCloudServer <- function(server) {
  server %in% c("posit.cloud", "rstudio.cloud")
}

isCloudServer <- function(server) {
  isPositCloudServer(server) || isShinyappsServer(server)
}

isSPCSServer <- function(server) {
  info <- serverInfo(server)
  grepl(pattern = "snowflakecomputing.app", x = info$url, fixed = TRUE)
}

checkCloudServer <- function(server, call = caller_env()) {
  if (!isCloudServer(server)) {
    cli::cli_abort("`server` must be shinyapps.io or posit.cloud", call = call)
  }
}

checkShinyappsServer <- function(server, call = caller_env()) {
  if (!isShinyappsServer(server)) {
    cli::cli_abort("`server` must be shinyapps.io", call = call)
  }
}

isRPubs <- function(server) {
  identical(server, "rpubs.com")
}

isConnectServer <- function(server) {
  !isCloudServer(server) && !isRPubs(server)
}

cloudServerInfo <- function(name, url) {
  list(
    name = name,
    url = getOption("rsconnect.shinyapps_url", url),
    certificate = inferCertificateContents(
      system.file("cert/shinyapps.io.pem", package = "rsconnect")
    )
  )
}

findServer <- function(server = NULL, local = TRUE, error_call = caller_env()) {
  if (!is.null(server)) {
    check_string(server, call = error_call)

    existing <- serverNames()
    if (!server %in% existing) {
      cli::cli_abort(c(
        "Can't find {.arg server} with name {.str {server}}.",
        i = "Known servers are {.str {existing}}."
      ))
    }
    server
  } else {
    existing <- servers(local = local)

    if (length(existing) == 0 || nrow(existing) == 0) {
      cli::cli_abort("No local servers have been registered")
    } else if (nrow(existing) == 1) {
      existing$name
    } else {
      idx <- cli_menu(
        "Multiple servers found.",
        "Which one do you want to use?",
        c(i = "Use {.arg server} to pick one of {.str {existing$name}}."),
        choices = existing$name
      )
      existing$name[idx]
    }
  }
}

#' Server management
#'
#' @description
#' These functions manage the list of known servers:
#'
#' * `addServer()` registers a Posit connect server. Once it has been
#'   registered, you can connect to an account on the server using
#'   [connectUser()].
#' * `removeServer()` removes a server from the registry.
#' * `addServerCertificate()` adds a certificate to a server.
#'
#' @param url URL for the server. Can be a bare hostname like
#'   `connect.mycompany.com` or a url like `http://posit.mycompany.com/connect`.
#' @param name Server name. If omitted, the server hostname is used.
#' @param certificate Optional. Either a path to certificate file or a
#'   character vector containing the certificate's contents.
#' @param validate Validate that `url` actually points to a Posit Connect
#'   server?
#' @param snowflakeConnectionName Name for the Snowflake connection parameters
#'   stored in `connections.toml`.
#' @param quiet Suppress output and prompts where possible.
#' @export
#' @examples
#' \dontrun{
#' # register a local server
#' addServer("http://myrsconnect/", "myserver")
#'
#' # list servers
#' servers(local = TRUE)
#'
#' # connect to an account on the server
#' connectUser(server = "myserver")
#' }
#' @export
addServer <- function(
  url,
  name = NULL,
  certificate = NULL,
  validate = TRUE,
  snowflakeConnectionName = NULL,
  quiet = FALSE
) {
  check_string(url)
  check_name(name, allow_null = TRUE)

  if (validate) {
    out <- validateConnectUrl(url, certificate, snowflakeConnectionName)
    if (!out$valid) {
      cli::cli_abort("{.arg url} does not appear to be a Posit Connect server.")
    }
    url <- out$url
  }

  name <- name %||% serverName(url)
  registerServer(name, url, certificate)

  if (!quiet) {
    message("Server '", name, "' added successfully: ", url)
  }
}


# Validate a connect server URL by hitting a known configuration endpoint
# The URL may be specified with or without the protocol and port; this function
# will try both http and https and follow any redirects given by the server.
validateConnectUrl <- function(
  url,
  certificate = NULL,
  snowflakeConnectionName = NULL
) {
  # Add protocol if missing, assuming https except for local installs
  if (!grepl("://", url, fixed = TRUE)) {
    if (grepl(":3939", url, fixed = TRUE)) {
      url <- paste0("http://", url)
    } else {
      url <- paste0("https://", url)
    }
  }
  url <- ensureConnectServerUrl(url)
  is_http <- grepl("^http://", url)

  GET_server_settings <- function(url) {
    timeout <- getOption("rsconnect.http.timeout", if (isWindows()) 20 else 10)
    auth_info <- list(certificate = inferCertificateContents(certificate))

    if (!is.null(snowflakeConnectionName)) {
      auth_info$snowflakeToken <- getSnowflakeAuthToken(
        url,
        snowflakeConnectionName
      )
    }
    GET(
      parseHttpUrl(url),
      auth_info,
      "/server_settings",
      timeout = timeout,
      rawResponse = TRUE
    )
  }
  response <- NULL
  cnd <- catch_cnd(response <- GET_server_settings(url), "error")

  if (is_http && cnd_inherits(cnd, "OPERATION_TIMEDOUT")) {
    url <- gsub("^http://", "https://", url)
    cnd <- catch_cnd(response <- GET_server_settings(url), "error")
  }

  if (!is.null(cnd)) {
    return(list(valid = FALSE, message = conditionMessage(cnd)))
  }

  contentType <- attr(response, "httpContentType")
  if (!isContentType(contentType, "application/json")) {
    return(list(valid = FALSE, message = "Endpoint did not return JSON"))
  }

  url <- gsub("/server_settings$", "", attr(response, "httpUrl"))
  list(valid = TRUE, url = url, response = response)
}

# Return a URL that can be concatenated with sub-paths like /content
ensureConnectServerUrl <- function(url) {
  # strip trailing /
  url <- gsub("/$", "", url)

  # ensure 'url' ends with '/__api__'
  if (!grepl("/__api__$", url)) {
    url <- paste(url, "/__api__", sep = "")
  }

  url
}

registerServer <- function(
  name,
  url,
  certificate = NULL,
  error_call = caller_env()
) {
  certificate <- inferCertificateContents(certificate)

  if (!identical(substr(url, 1, 5), "https") && !is.null(certificate)) {
    cli::cli_abort(
      c(
        "Certificates may only be attached to servers that use the HTTPS protocol.",
        i = "Specify an HTTPS URL for the server, or omit the certificate."
      ),
      call = error_call
    )
  }

  path <- serverConfigFile(name)
  fields <- list(
    name = name,
    url = url,
    certificate = certificate %||% NA
  )
  write.dcf(fields, path, keep.white = "certificate")
}

#' @rdname addServer
#' @export
removeServer <- function(name = NULL) {
  name <- findServer(name)

  configFile <- serverConfigFile(name)
  unlink(configFile)
}

#' @rdname addServer
#' @export
addServerCertificate <- function(name, certificate, quiet = FALSE) {
  info <- serverInfo(name)
  registerServer(name, info$url, certificate)

  if (!quiet) {
    message("Certificate added to server '", name, "'")
  }

  invisible(NULL)
}

serverName <- function(url) {
  url <- parseHttpUrl(url)
  paste0(url$host, if (nchar(url$port) > 0) ":", url$port)
}
