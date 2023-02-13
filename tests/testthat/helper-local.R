fakeAccounts <- function(accounts, servers, env = parent.frame()) {
  force(accounts)
  force(servers)

  function(server = NULL) {
    df <- data.frame(
      name = accounts,
      server = servers,
      stringsAsFactors = FALSE
    )
    if (!is.null(server)) {
      df <- df[df$server == server, , drop = FALSE]
    }
    df
  }
}

fakeAccountInfo <- function(...) {
  info <- list(...)

  function(name, server = NULL) {
    info[[name]]
  }
}
