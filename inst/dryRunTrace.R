rsconnect_log <- function(...) {
  cat(paste0("[dryRun] ", ..., "\n", collapse = ""), file = stderr())
}
traceFun <- function(ns, fun, code) {
  if (ns == "base") {
    where <- baseenv()
  } else {
    where <- asNamespace(ns)
  }

  suppressMessages(
    trace(fun, substitute(code), where = where, print = FALSE)
  )
  invisible()
}

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
traceFun("utils", "install.packages", stop("Shouldn't install on server"))


# traceFun("rsconnect", "deployApp", stop("Apps can't deploy apps"))
traceFun("base", "browser", rsconnect_log("Can't use browser(); no interactive session"))

traceFun("utils", "browseURL", {
  rsconnect_log("Attempting to browse to <", url, ">")
})

# need to do in onLoad hooks



# traceFun("rstudioapi", "askForPassword", stop("Cant't get password"))
