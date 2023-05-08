rsconnect_log <- function(...) {
  cat(paste0("[dryRun] ", ..., "\n", collapse = ""), file = stderr())
}
traceFun <- function(ns, fun, code) {
  traceFun_(ns, fun, substitute(code))
}
traceFun_ <- function(ns, fun, code) {
  if (ns == "base") {
    where <- baseenv()
  } else {
    where <- asNamespace(ns)
  }

  suppressMessages(
    trace(fun, code, where = where, print = FALSE)
  )
  invisible()
}

# Messages ----------------------------------------------------------------

env_seen <- new.env(parent = emptyenv())
env_seen$PATH <- TRUE
env_seen$TESTTHAT <- TRUE
env_seen$RSTUDIO <- TRUE

traceFun("base", "Sys.getenv", {
  if (!grepl("^(_R|R|RSTUDIO|CALLR|CLI|RENV|RMARKDOWN)_", x)) {
    if (!exists(x, envir = env_seen)) {
      rsconnect_log("Getting env var '", x, "'")
      env_seen[[x]] <- TRUE
    }
  }
})
traceFun("base", ".libPaths", {
  if (!missing(new)) {
    rsconnect_log("Updating .libPaths")
  }
})

traceFun("base", "browser", {
  rsconnect_log("Can't use browser(); no interactive session")
})

traceFun("utils", "browseURL", {
  rsconnect_log("Attempting to browse to <", url, ">")
})


# Errors ------------------------------------------------------------------

errorOnServer <- function(ns, fun, reason) {
  code <- substitute(
    stop(paste0("[dryRun] `", ns, "::", fun, "()`: ", reason), call. = FALSE)
  )
  traceFun_(ns, fun, code)
}

errorOnServer(
  "utils",
  "install.packages",
  "install packages locally, not on the server"
)

setHook(
  packageEvent("rstudioapi", "onLoad"),
  errorOnServer(
    "rsconnect",
    "deployApp",
    "apps shouldn't deploy apps on the server"
  )
)

setHook(
  packageEvent("rstudioapi", "onLoad"),
  errorOnServer(
    "rstudioapi",
    "askForPassword",
    "can't interactively ask for password on server"
  )
)
