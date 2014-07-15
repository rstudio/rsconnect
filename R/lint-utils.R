stripComments <- function(content) {
  gsub("#.*", "", content, perl = TRUE)
}

hasAbsolutePaths <- function(content) {
  regex <- c(
    "[\'\"]\\s*[a-zA-Z]:", ## windows-style absolute paths
    "[\'\"]\\s*/(.*?)/(.*?)" ## unix-style absolute paths
  )
  results <- as.logical(Reduce(`+`, lapply(regex, function(rex) {
    grepl(rex, content, perl = TRUE)
  })))
}

hasBadRelativePaths <- function(content) {
  
}

enumerate <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  result <- vector("list", length(X))
  for (i in seq_along(X)) {
    result[[i]] <- FUN(X[[i]], i, ...)
  }
  names(result) <- names(X)
  result
}

makeLinterMessage <- function(header, content, lines) {
  c(header,
    paste(lines, ": ", content[lines], sep = ""),
    "\n")
}