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

  if (isCloudServer(name)) {
    info <- cloudServerInfo(name)
  } else {
    configFile <- serverConfigFile(name)
    serverDcf <- readDcf(serverConfigFile(name), all = TRUE)
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

cloudServers <- c("shinyapps.io", "posit.cloud", "rstudio.cloud")

isCloudServer <- function(server) {
  server %in% cloudServers
}

isShinyappsServer <- function(server) {
  identical(server, "shinyapps.io")
}

isRPubs <- function(server) {
  identical(server, "rpubs.com")
}

isConnectServer <- function(server) {
  !isCloudServer(server) && !isRPubs(server)
}

cloudServerInfo <- function(name) {
  name <- arg_match0(name, cloudServers)

  list(
    name = name,
    url = getOption("rsconnect.shinyapps_url", "https://api.shinyapps.io/v1"),
    certificate = inferCertificateContents(
      system.file("cert/shinyapps.io.pem", package = "rsconnect")
    )
  )
}

findServer <- function(server = NULL,
                       local = TRUE,
                       error_call = caller_env()) {

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
#' @param name Optional nickname for the server. If none is given, the nickname
#'   is inferred from the server's hostname.
#' @param certificate Optional. Either a path to certificate file or a
#'   character vector containing the certificate's contents.
#' @param validate Validate that `url` actually points to a Posit Connect
#'   server?
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
addServer <- function(url, name = NULL, certificate = NULL, validate = TRUE, quiet = FALSE) {
  check_string(url)
  check_name(name, allow_null = TRUE)

  if (validate) {
    out <- validateConnectUrl(url, certificate)
    if (!out$valid) {
      cli::cli_abort("{.arg url} does not appear to be a Posit Connect server.")
    }
    url <- out$url
  }

  serverUrl <- parseHttpUrl(url)

  # if no name is supplied for the server, make one up based on the host portion
  # of its URL
  if (is.null(name)) {
    name <- serverUrl$host
    if (!quiet && is_interactive()) {
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

# Return a URL that can be concatenated with sub-paths like /content
ensureConnectServerUrl <- function(url) {
  # strip trailing /
  url <- gsub("/$", "", url)

  # ensure 'url' ends with '/__api__'
  if (!grepl("/__api__$", url))
    url <- paste(url, "/__api__", sep = "")

  url
}
