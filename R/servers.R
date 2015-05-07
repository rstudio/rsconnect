#' Server Management Functions
#'
#' Functions to manage the list of known servers to which
#' \pkg{rsconnect} can deploy and manage applications.
#'
#' Register a server with \code{addServer} or \code{discoverServers} (the latter
#' is useful only if your administrator has configured server autodiscovery).
#' Once a server is registered, you can connect to an account on the server
#' using \code{\link{connectUser}}.
#'
#' The \code{servers} and \code{serverInfo} functions are provided for viewing
#' previously registered servers.
#'
#' There is always at least one server registered (the \code{shinyapps.io}
#' server)
#'
#' @param name Optional nickname for the server. If none is given, the nickname
#'   is inferred from the server's hostname.
#' @param url Server's URL. Should look like \code{http://servername/} or
#'  \code{http://servername:port/}.
#' @param local Return only local servers (i.e. not \code{shinyapps.io})
#' @param quiet Suppress output and prompts where possible.
#' @return
#' \code{servers} returns a data frame with registered server names and URLs.
#' \code{serverInfo} returns a list with details for a particular server.
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
  configFiles <- list.files(serverConfigDir(), pattern=glob2rx("*.dcf"),
                            full.names = TRUE)
  parsed <- lapply(configFiles, read.dcf)
  locals <- do.call(rbind, parsed)
  if (local) {
    locals
  } else {
    rbind(locals, as.data.frame(shinyappsServerInfo(), stringsAsFactors = FALSE))
  }
}

serverConfigDir <- function() {
  rsconnectConfigDir("servers")
}

serverConfigFile <- function(name) {
  normalizePath(file.path(serverConfigDir(), paste(name, ".dcf", sep="")),
                mustWork = FALSE)
}

shinyappsServerInfo <- function() {
  info <- list(name = "shinyapps.io",
               url = getOption("rsconnect.shinyapps_url", "https://api.shinyapps.io/v1"))
}

#' @rdname servers
#' @export
discoverServers <- function(quiet = FALSE) {
  # TODO: Better discovery mechanism?
  discovered <- getOption("rsconnect.local_servers", "http://localhost:3939/__api__")

  # get the URLs of the known servers, and silently add any that aren't yet
  # present
  existing <- servers()[,"url"]
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

getDefaultServer <- function(local = FALSE, prompt = TRUE) {
   existing <- servers(local)
   # if there are no existing servers, silently try to discover one to work with
   if (length(existing) == 0 || nrow(existing) == 0) {
     discoverServers(quiet = TRUE)
     existing <- servers(local)
   }

   # if exactly one server exists, return it
   if (nrow(existing) == 1) {
     return(list(name = as.character(existing[,"name"]),
                 url = as.character(existing[,"url"])))
   }

   # no default server, prompt if there are multiple choices
  if (nrow(existing) > 1 && prompt && interactive()) {
    name <- as.character(existing[1,"name"])
    message("Registered servers: ", paste(existing[,"name"], collapse = ", "))
    input <- readline(paste0(
      "Which server (default '", name ,"')? "))
    if (nchar(input) > 0) {
      name <- input
    }
    return(serverInfo(name))
  }
}

#' @rdname servers
#' @export
addConnectServer <- function(url, name = NULL, quiet = FALSE) {
  addServer(ensureConnectServerUrl(url), name, quiet)
}

#' @rdname servers
#' @export
addServer <- function(url, name = NULL, quiet = FALSE) {
  if (!isStringParam(url))
    stop(stringParamErrorMessage("url"))

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

  # write the server info
  configFile <- serverConfigFile(name)
  write.dcf(list(name = name,
                 url = url),
            configFile)

  if (!quiet) {
    message("Server '", name,  "' added successfully: ", url)
  }
}

#' @rdname servers
#' @export
removeServer <- function(name) {
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))
  configFile <- serverConfigFile(name)
  if (file.exists(configFile))
    unlink(configFile)
  else
    warning("The server '", name,"' is not currently registered.")
}


#' @rdname servers
#' @export
serverInfo <- function(name) {
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))

  # there's no config file for shinyapps.io
  if (identical(name, "shinyapps.io")) {
    return(shinyappsServerInfo())
  }

  configFile <- serverConfigFile(name)
  if (!file.exists(configFile))
    stop(missingServerErrorMessage(name))

  serverDcf <- readDcf(serverConfigFile(name), all=TRUE)
  info <- as.list(serverDcf)
  info
}

missingServerErrorMessage <- function(name) {
  paste0("server named '", name, "' does not exist")
}

clientForAccount <- function(account) {

  if (account$server == shinyappsServerInfo()$name)
    lucidClient(shinyappsServerInfo()$url, account)
  else {
    server <- serverInfo(account$server)
    connectClient(server$url, account)
  }
}

ensureConnectServerUrl <- function(url) {
  # ensure 'url' ends with '/__api__'
  if (!grepl("/__api__$", url))
    url <- paste(url, "/__api__", sep = "")

  # if we have duplicated leading slashes, remove them
  url <- gsub("(/+__api__)$", "/__api__", url)
  url
}
