
stripComments <- function(content) {
  gsub("#.*", "", content, perl = TRUE)
}

hasAbsolutePaths <- function(content) {

  regex <- c(
    "\\[(.*?)\\]\\(\\s*[a-zA-Z]:/[^\"\']", ## windows-style markdown references [Some image](C:/...)
    "\\[(.*?)\\]\\(\\s*/[^/]", ## unix-style markdown references [Some image](/Users/...)
    NULL ## so we don't worry about commas above
  )

  regexResults <- as.logical(Reduce(`+`, lapply(regex, function(rex) {
    grepl(rex, content, perl = TRUE)
  })))

  # Strip out all strings in the document, and check to see if any of them
  # resolve to absolute paths on the system.
  sQuoteRegex <- "['](?:(?:\\\\.)|(?:[^'\\\\]))*?[']"
  dQuoteRegex <- '["](?:(?:\\\\.)|(?:[^"\\\\]))*?["]'

  extractedStrings <- lapply(c(sQuoteRegex, dQuoteRegex), function(regex) {
    matches <- gregexpr(regex, content, perl = TRUE)
    lapply(seq_along(matches), function(i) {
      match <- matches[[i]]
      if (c(match[[1]]) == -1L) return(character())
      starts <- as.integer(match) + 1
      ends <- starts + attr(match, "match.length") - 3
      substring(content[[i]], starts, ends)
    })
  })

  strings <- vector("list", length(extractedStrings[[1]]))
  for (i in seq_along(extractedStrings[[1]])) {
    strings[[i]] <- unique(c(extractedStrings[[1]][[i]], extractedStrings[[2]][[i]]))
    strings[[i]] <- strings[[i]][nchar(strings[[i]]) >= 5]
  }

  lineHasAbsolutePath <- unlist(lapply(strings, function(x) {
    any(
      grepl("^/|^[a-zA-Z]:/|^~", x, perl = TRUE) &
      file.exists(x) &
      file.info(x)$isdir %in% FALSE &
      vapply(gregexpr("[~/]", x, perl = TRUE), USE.NAMES = FALSE, FUN.VALUE = numeric(1), length) >= 3
    )
  }))

  as.logical(lineHasAbsolutePath + regexResults)

}

noMatch <- function(x) {
  identical(attr(x, "match.length"), -1L)
}

badRelativePaths <- function(content, project, path, ...) {

  ## Figure out how deeply the path of the file is nested
  ## (it is relative to the project root)
  slashMatches <- gregexpr("/", path)
  nestLevel <- if (noMatch(slashMatches)) 0 else length(slashMatches[[1]])

  ## Identify occurrences of "../"
  regexResults <- gregexpr("../", content, fixed = TRUE)

  ## Figure out sequential runs of `../`
  runs <- lapply(regexResults, function(x) {
    if (noMatch(x)) return(NULL)
    rle <- rle(as.integer(x) - seq(0, by = 3, length.out = length(x)))
    rle$lengths
  })

  badPaths <- vapply(runs, function(x) {
    any(x > nestLevel)
  }, logical(1))

  badPaths
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

#' Construct a Linter Message
#'
#' Pretty-prints a linter message. Primarily used as a helper
#' for constructing linter messages with \code{\link{linter}}.
#'
#' @param header A header message describing the linter.
#' @param content The content of the file that was linted.
#' @param lines The line numbers from \code{content} that contain lint.
makeLinterMessage <- function(header, content, lines) {

  lint <- attr(lines, "lint")

  c(
    paste0(header, ":"),
    paste(lines, ": ",
          content[lines],
          if (!is.null(lint)) paste("    ", lint, sep = ""),
          sep = ""),
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

transposeList <- function(list) {
  unname(as.list(
    as.data.frame(
      t(
        as.matrix(
          as.data.frame(list, stringsAsFactors = FALSE)
        )
      ), stringsAsFactors = FALSE)
  ))
}

