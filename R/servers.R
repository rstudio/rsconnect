#' Server Management Functions
#'
#' Functions to manage the list of known servers to which
#' \pkg{rsconnect} can deploy and manage applications.
#'
#' Register a server with `addServer` or `discoverServers` (the latter
#' is useful only if your administrator has configured server autodiscovery).
#' Once a server is registered, you can connect to an account on the server
#' using [connectUser()].
#'
#' The `servers` and `serverInfo` functions are provided for viewing
#' previously registered servers.
#'
#' Servers for `shinyapps.io` and `posit.cloud` are always registered.
#'
#' @param name Optional nickname for the server. If none is given, the nickname
#'   is inferred from the server's hostname.
#' @param url Server's URL. Should look like `http://servername/` or
#'  `http://servername:port/`.
#' @param local Return only local servers (i.e. not `shinyapps.io`)
#' @param certificate Optional; a path a certificate file to be used when making
#'   SSL connections to the server. The file's contents are copied and stored by
#'   the \pkg{rsconnect} package. Can also be a character vector containing the
#'   certificate's contents.
#' @param quiet Suppress output and prompts where possible.
#' @return
#' `servers` returns a data frame with registered server names and URLs.
#' `serverInfo` returns a list with details for a particular server.
#' @rdname servers
#' @examples
#' \dontrun{
#'
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
servers <- function(local = FALSE) {
  configFiles <- serverConfigFiles()
  parsed <- lapply(configFiles, function(file) {
    info <- read.dcf(file)

    # empty if no contents
    if (identical(nrow(info), 0L))
      return(NULL)

    # return parsed server info
    info
  })

  parsed <- lapply(parsed, as.data.frame, stringsAsFactors = FALSE)
  locals <- rbind_fill(parsed, c("name", "url", "certificate"))

  if (local) {
    out <- locals
  } else {
    out <- rbind(
      locals,
      as.data.frame(shinyappsServerInfo(), stringsAsFactors = FALSE),
      as.data.frame(cloudServerInfo(), stringsAsFactors = FALSE))

    # RStudio IDE requires a server whose name matches the server name on
    # previously configured accounts. Prevent breakage for pre-rebrand users.
    if (nrow(accounts(server = "rstudio.cloud")) > 0) {
      out <- rbind(
        out,
        as.data.frame(cloudServerInfo("rstudio.cloud"), stringsAsFactors = FALSE)
      )
    }
  }
  out$certificate <- secret(out$certificate)
  out
}

shinyappsServerInfo <- function() {
  info <- list(name = "shinyapps.io",
               certificate = inferCertificateContents(
                 system.file("cert/shinyapps.io.pem", package = "rsconnect")),
               url = getOption("rsconnect.shinyapps_url",
                               "https://api.shinyapps.io/v1"))
}

cloudServerInfo <- function(name = "posit.cloud") {
  # We encode the current and prior product names here and call this function to
  # see if a configured server identifier references the cloud product.
  if (!is.element(name, c("posit.cloud", "rstudio.cloud"))) {
    name <- "posit.cloud"
  }
  info <- list(name = name,
               certificate = inferCertificateContents(
                 system.file("cert/shinyapps.io.pem", package = "rsconnect")),
               url = getOption("rsconnect.shinyapps_url",
                               "https://api.shinyapps.io/v1"))
}

#' @rdname servers
#' @export
discoverServers <- function(quiet = FALSE) {
  # TODO: Better discovery mechanism?
  discovered <- getOption("rsconnect.local_servers", "http://localhost:3939/__api__")

  # get the URLs of the known servers, and silently add any that aren't yet
  # present
  existing <- servers()[, "url"]
  introduced <- setdiff(discovered, existing)
  lapply(introduced, function(url) { addServer(url, quiet = TRUE) })

  if (!quiet && length(introduced) > 0) {
    message("Discovered ", length(introduced),
            (if (length(introduced) == 1) "server" else "servers"), ":")
    lapply(introduced, message)
  } else if (!quiet) {
    message("No new servers found.")
  }
  invisible(introduced)
}

findServer <- function(server = NULL,
                       error_call = caller_env()) {

  if (!is.null(server)) {
    check_string(server, call = error_call)

    existing <- servers()
    if (!server %in% existing$name) {
      cli::cli_abort(c(
        "Can't find {.arg server} with name {.str {server}}.",
        i = "Known servers are {.str {existing$name}}."
      ))
    }
    server
  } else {
    existing <- servers(local = TRUE)

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

#' @rdname servers
#' @export
addConnectServer <- function(url, name = NULL, certificate = NULL,
                             quiet = FALSE) {
  addServer(ensureConnectServerUrl(url), name, certificate, quiet)
}

#' @rdname servers
#' @export
addServer <- function(url, name = NULL, certificate = NULL, quiet = FALSE) {
  check_string(url)
  check_name(name, allow_null = TRUE)

  serverUrl <- parseHttpUrl(url)

  # TODO: test server by hitting URL and getting config?

  # if no name is supplied for the server, make one up based on the host portion
  # of its URL
  if (is.null(name)) {
    name <- serverUrl$host
    if (!quiet && interactive()) {
      input <- readline(paste0(
        "Enter a nickname for this server (default '", name, "'): "))
      if (nchar(input) > 0) {
        name <- input
      }
    }
  }

  if (!identical(serverUrl$protocol, "https") &&
      !is.null(certificate) && nzchar(certificate)) {
    stop("Certificates may only be attached to servers that use the ",
         "HTTPS protocol. Sepecify an HTTPS URL for the server, or ",
         "omit the certificate.")
  }

  # resolve certificate argument
  certificate <- inferCertificateContents(certificate)

  # write the server info
  configFile <- serverConfigFile(name)
  if (is.null(certificate)) {
    # no certificate, just write name and URL for brevity
    write.dcf(list(name = name,
                   url = url),
              configFile)
  } else {
    # write all fields
    write.dcf(list(name = name,
                   url = url,
                   certificate = certificate),
              configFile,
              keep.white = "certificate")
  }

  if (!quiet) {
    message("Server '", name,  "' added successfully: ", url)
  }
}

#' @rdname servers
#' @export
removeServer <- function(name) {
  check_string(name)

  configFile <- serverConfigFile(name)
  if (file.exists(configFile))
    unlink(configFile)
  else
    warning("The server '", name, "' is not currently registered.")
}


#' @rdname servers
#' @export
serverInfo <- function(name) {
  check_string(name)

  # there's no config file for Posit's hosted offerings
  if (identical(name, "shinyapps.io")) {
    return(shinyappsServerInfo())
  }

  if (identical(name, cloudServerInfo(name)$name)) {
    return(cloudServerInfo(name))
  }

  configFile <- serverConfigFile(name)
  if (!file.exists(configFile))
    stop(missingServerErrorMessage(name))

  serverDcf <- readDcf(serverConfigFile(name), all = TRUE)
  info <- as.list(serverDcf)
  info
}

#' @rdname servers
#' @export
addServerCertificate <- function(name, certificate, quiet = FALSE) {
  # read the existing server information (throws an error on failure)
  info <- serverInfo(name)

  if (!identical(substr(info$url, 1, 5), "https")) {
    stop("Certificates may only be attached to servers that use the ",
         "HTTPS protocol. Sepecify an HTTPS URL for the server, or ",
         "omit the certificate.")
  }

  # append the certificate and re-write the server information
  info$certificate <- inferCertificateContents(certificate)
  write.dcf(info, serverConfigFile(name), keep.white = "certificate")

  if (!quiet)
    message("Certificate added to server '", name, "'")

  invisible(NULL)
}

missingServerErrorMessage <- function(name) {
  paste0("server named '", name, "' does not exist")
}

# Return a URL that can be concatenated with sub-paths like /content
ensureConnectServerUrl <- function(url) {
  # strip trailing /
  url <- gsub("/$", "", url)

  # ensure 'url' ends with '/__api__'
  if (!grepl("/__api__$", url))
    url <- paste(url, "/__api__", sep = "")

  url
}
