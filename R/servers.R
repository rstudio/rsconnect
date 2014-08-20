serverConfigDir <- function() {
  rsconnectConfigDir("servers")
}

serverConfigFile <- function(name) {
  normalizePath(file.path(serverConfigDir(), paste(name, ".dcf", sep="")),
                mustWork = FALSE)
}

.lucidServerInfo <- list(
  name = "shinyapps.io",
  url = "https://api.shinyapps.io/v1")

#' @export
servers <- function(local = FALSE) {
  configFiles <- list.files(serverConfigDir(), pattern=glob2rx("*.dcf"),
                            full.names = TRUE)
  parsed <- lapply(configFiles, read.dcf)
  locals <- do.call(rbind, parsed)
  if (local) {
    locals
  } else {
    rbind(locals, as.data.frame(.lucidServerInfo, stringsAsFactors = FALSE))
  }
}

#' @export
discoverServers <- function(quiet = FALSE) {
  # TODO: Better discovery mechanism?
  discovered <- getOption("rsconnect.local_servers", "http://localhost:8082")

  # get the URLs of the known servers, and silently add any that aren't yet
  # present
  existing <- servers()[,"url"]
  introduced <- setdiff(discovered, existing)
  lapply(introduced, function(url) { addServer(url, quiet = TRUE) })

  if (!quiet && length(introduced) > 0) {
    message("Discovered ", length(introduced), " RStudio Connect ",
            (if (length(introduced) == 1) "server" else "servers"), ":")
    lapply(introduced, message)
  } else if (!quiet) {
    message("No new RStudio Connect servers found.")
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


#' @export
serverInfo <- function(name) {
  if (!isStringParam(name))
    stop(stringParamErrorMessage("name"))

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
  if (account$server == .lucidServerInfo$name)
    lucidClient(.lucidServerInfo$url, account)
  else {
    server <- serverInfo(account$server)
    connectClient(server$url, account)
  }
}
