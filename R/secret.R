secret <- function(x) {
  if (is.null(x)) return(NULL)

  stopifnot(is.character(x) || all(is.na(x)))
  structure(x, class = "rsconnect_secret")
}

#' @export
format.rsconnect_secret <- function(x, ...) {
  paste0(substr(x, 1, 6), "... (redacted)")
}

#' @export
print.rsconnect_secret <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
str.rsconnect_secret <- function(object, ...) {
  cat(" ", format(object), "\n", sep = "")
}

#' @export
as.data.frame.rsconnect_secret <- function(x,
                                           ...) {
  structure(
    list(x),
    row.names = .set_row_names(length(x)),
    class = "data.frame"
  )
}
