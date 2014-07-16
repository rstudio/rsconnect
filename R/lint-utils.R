stripComments <- function(content) {
  gsub("#.*", "", content, perl = TRUE)
}

hasAbsolutePaths <- function(content) {
  regex <- c(
    "[\'\"]\\s*[a-zA-Z]:", ## windows-style absolute paths
    "[\'\"]\\s*\\\\\\\\", ## windows UNC paths
    "[\'\"]\\s*/(.*?)/(.*?)", ## unix-style absolute paths
    "[\'\"]\\s*~/", ## path to home directory
    "\\[.*?\\]\\(\\s*[a-zA-Z]:", ## windows-style markdown references [Some image](C:/...)
    "\\[.*?\\]\\(\\s*[a-zA-Z]:", ## unix-style markdown references [Some image](/Users/...)
    NULL ## so we don't worry about commas above
  )
  results <- as.logical(Reduce(`+`, lapply(regex, function(rex) {
    grepl(rex, content, perl = TRUE)
  })))
}

badRelativePaths <- function(content, project, path) {
  nestLevel <- length(gregexpr("/", path)[[1]])
  regexResult <- gregexpr("../", content, fixed = TRUE)
  results <- unlist(lapply(regexResult, function(x) {
    if (identical(attr(x, "match.length"), -1L))
      return(FALSE)
    return(length(x) > nestLevel)
  }))
  results
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
  c(
    paste0(header, ":"),
    paste(lines, ": ", content[lines], sep = ""),
    "\n"
  )
}

hasLint <- function(x) {
  any(unlist(lapply(x, function(x) {
    lapply(x, function(x) {
      length(x$indices) > 0
    })
  })))
}

isRCodeFile <- function(path) {
  grepl("\\.[rR]$|\\.[rR]md$|\\.[rR]nw$", path)
}