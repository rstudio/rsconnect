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

addLinter("invalid.relative.paths", linter(

  apply = function(content, ...) {
    content <- stripComments(content)
    badRelativePaths(content, ...)
  },

  takes = isRCodeFile,

  message = function(content, lines) {
    makeLinterMessage("The following lines contain invalid relative paths (resolved outside of project directory)",
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
                      shQuote(from[[regex]][[i]]),
                      " -> ",
                      shQuote(to[[regex]][[i]]),
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

