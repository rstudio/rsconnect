#' Discover servers automatically
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has never worked usefully, so has been removed.
#'
#' @export
#' @keywords internal
discoverServers <- function(quiet = FALSE) {
  lifecycle::deprecate_warn("0.9.0", "discoverServers()")

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
