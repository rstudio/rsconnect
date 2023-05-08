.__LINTERS__. <- new.env(parent = emptyenv())

##' Add a Linter
##'
##' Add a linter, to be used in subsequent calls to [lint()].
##'
##' @param name The name of the linter, as a string.
##' @param linter A [linter()].
##' @export
##' @example examples/example-linter.R
addLinter <- function(name, linter) {
  assign(name, linter, envir = .__LINTERS__.)
}

##' Create a Linter
##'
##' Generate a linter, which can identify errors or problematic regions in a
##' project.
##'
##' @param apply Function that, given the content of a file, returns the indices
##'   at which problems were found.
##' @param takes Function that, given a set of paths, returns the subset of
##'   paths that this linter uses.
##' @param message Function that, given content and lines, returns an
##'   informative message for the user. Typically generated with
##'   [makeLinterMessage()].
##' @param suggestion String giving a prescribed fix for the linted problem.
##' @export
##' @example examples/example-linter.R
linter <- function(apply, takes, message, suggestion) {
  result <- list(
    apply = apply,
    takes = takes,
    message = message,
    suggestion = suggestion
  )
  class(result) <- "linter"
  result
}

getLinterApplicableFiles <- function(linter, files) {
  result <- linter$takes(files)
  if (is.numeric(result) || is.logical(result)) {
    files[result]
  } else {
    result
  }
}

applyLinter <- function(linter, ...) {
  result <- linter$apply(...)
  if (is.logical(result)) {
    output <- which(result)
  } else {
    output <- as.numeric(result)
  }
  attributes(output) <- attributes(result)
  output
}


isRCodeFile <- function(path) {
  grepl("\\.[rR]$|\\.[rR]md$|\\.[rR]nw$", path)
}


addLinter("absolute.paths", linter(

  apply = function(content, ...) {
    content <- stripComments(content)
    which(hasAbsolutePaths(content))
  },

  takes = isRCodeFile,

  message = function(content, lines) {
    makeLinterMessage("The following lines contain absolute paths",
                      content,
                      lines)
  },

  suggestion = "Paths should be to files within the project directory."
))

addLinter("filepath.capitalization", linter(

  apply = function(content, project, path, files) {
    content <- stripComments(content)

    # Extract references between bounding (unescaped) quotes.
    extractQuoted <- function(regex) {
      matches <- gregexpr(regex, content, perl = TRUE)
      results <- vector("list", length(content))
      for (i in seq_along(matches)) {
        x <- matches[[i]]
        if (x[[1]] == -1L || length(x) %% 2 != 0) next
        starts <- x[seq(1, length(x), by = 2)]
        ends   <- x[seq(2, length(x), by = 2)]
        results[[i]] <- character(length(starts))
        for (j in seq_along(starts)) {
          results[[i]][[j]] <- substring(content[i], starts[j] + 1, ends[j] - 1)
        }
      }
      results
    }

    # Extract targets from Markdown links.
    extractLinked <- function() {
      regex <- "\\]\\(.*?\\)"
      matches <- gregexpr(regex, content, perl = TRUE)
      results <- vector("list", length(content))
      for (i in seq_along(matches)) {
        x <- matches[[i]]
        if (x[[1]] == -1L) next
        results[[i]] <- character(length(x))
        attr <- attributes(x)
        for (j in seq_along(x)) {
          raw <- x[[j]]
          raw.length <- attr$match.length[[j]]
          # skip past "](" and discard ")"
          start <- as.integer(raw) + 2
          end <- as.integer(raw) + raw.length - 2
          results[[i]][[j]] <- substring(content[i], start, end)
        }
      }
      results
    }

    # Inferred files within source documents; between matching quotes or in
    # something that looks like a Markdown link.
    inferredFiles <- list(
      "single.quotes" = extractQuoted("(?!\\\\)\'"),
      "double.quotes" = extractQuoted("(?!\\\\)\""),
      "markdown.links" = extractLinked()
    )

    ## Replace '\' with '/' in filepaths for consistency in comparisons
    inferredFiles <- lapply(inferredFiles, function(x) {
      lapply(x, function(xx) {
        gsub("\\\\", "/", xx, perl = TRUE)
      })
    })

    # Compare in case sensitive, case insensitive fashion
    projectFiles <- files
    projectFilesLower <- tolower(files)

    badLines <- lapply(inferredFiles, function(regex) {
      lapply(regex, function(x) {

        which(
          # Only examine paths containing a slash to reduce false positives
          grepl("/", x, fixed = TRUE) &
          (tolower(x) %in% projectFilesLower) &
            (!(x %in% projectFiles))
        )

      })
    })

    indices <- Reduce(union, lapply(badLines, function(x) {
      which(sapply(x, length) > 0)
    }))

    if (!length(indices)) return(integer())

    from <- lapply(inferredFiles, function(x) x[indices])
    to <- lapply(from, function(x) {
      lapply(x, function(xx) {
        projectFiles[tolower(xx) == projectFilesLower]
      })
    })

    messages <- lapply(seq_along(from), function(regex) {
      lapply(seq_along(regex), function(i) {
        if (length(from[[regex]][[i]]))
          paste(collapse = ", ",
                paste("[",
                      sQuote(from[[regex]][[i]]),
                      " -> ",
                      sQuote(to[[regex]][[i]]),
                      "]", sep = "")
          )
        else
          ""
      })
    })

    transposed <- transposeList(messages)
    lint <- sapply(transposed, function(x) {
      paste(x[x != ""], collapse = ", ")
    })

    indices <- as.numeric(indices)
    attr(indices, "lint") <- lint
    indices

  },

  takes = isRCodeFile,

  message = function(content, lines) {
    makeLinterMessage(
      "The following lines contain paths to files not matching in case sensitivity",
      content,
      lines
    )
  },

  suggestion = "Filepaths are case-sensitive on deployment server."

))

addLinter("browser", linter(
  apply = function(content, ...) {
    content <- stripComments(content)
    which(hasBrowserCalls(content))
  },

  takes = isRCodeFile,

  message = function(content, lines) {
    makeLinterMessage("The following lines contain the browser() debugging function",
                      content,
                      lines)
  },

  suggestion = "The browser() debugging function should be removed."
))

addLinter("browseURL", linter(
  apply = function(content, ...) {
    content <- stripComments(content)
    which(hasBrowseURLCalls(content))
  },

  takes = isRCodeFile,

  message = function(content, lines) {
    makeLinterMessage("The following lines contain calls to the browseURL function",
                      content,
                      lines)
  },

  suggestion = "Remove browseURL calls; browseURL does not work in deployed applications."
))


stripComments <- function(content) {
  gsub("#.*", "", content, perl = TRUE)
}

hasBrowserCalls <- function(content) {
  # look for calls to browser(); they will cause a debug halt, which is almost
  # never wanted in a production application
  grepl("\\bbrowser\\([^)]*\\)", content, perl = TRUE)
}

hasBrowseURLCalls <- function(content) {
  # look for calls to browseURL(); browsers can't be opened on the server in
  # deployed applications
  grepl("\\bbrowseURL\\([^)]*\\)", content, perl = TRUE)
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
      !dirExists(x) &
      vapply(gregexpr("[~/]", x, perl = TRUE), USE.NAMES = FALSE, FUN.VALUE = numeric(1), length) >= 3
    )
  }))

  as.logical(lineHasAbsolutePath + regexResults)

}

noMatch <- function(x) {
  identical(attr(x, "match.length"), -1L)
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
