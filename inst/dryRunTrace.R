rsconnect_log <- function(...) {
  cat(paste0("[rsconnect] ", ..., "\n", collapse = ""), file = stderr())
}
traceFun <- function(ns, fun, code) {
  if (ns == "base") {
    where <- baseenv()
  } else {
    where <- asNamespace(ns)
  }

  trace(fun, substitute(code), where = where, print = FALSE)
  invisible()
}

traceFun("base", "Sys.getenv", rsconnect_log("Getting env var '", x, "'"))
traceFun("base", ".libPaths", {
  if (!missing(new)) {
    rsconnect_log("Updating .libPaths")
  }
})
traceFun("utils", "install.packages", stop("Shouldn't install on server"))


traceFun("rsconnect", "deployApp", stop("Apps can't deploy apps"))
traceFun("base", "browser", rsconnect_log("Can't use browser(); no interactive session"))

traceFun("utils", "browseURL", {
  rsconnect_log("Attempting to browse to <", url, ">")
})

# need to do in onLoad hooks



traceFun("rstudioapi", "askForPassword", stop("Cant't get password"))
