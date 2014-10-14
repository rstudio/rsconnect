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
    
    # Inferred files within source documents (really, we just
    # extract everything between two quotes)
    inferredFiles <- lapply(c("(?!\\\\)\'", "(?!\\\\)\""), function(regex) {
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
    })
    
    ## Replace '\' with '/' in filepaths for consistency in comparisons
    inferredFiles <- lapply(inferredFiles, function(x) {
      gsub("\\\\", "/", x, perl = TRUE)
    })
    
    # Compare in case sensitive, case insensitive fashion
    projectFiles <- files
    projectFilesLower <- tolower(files)
    
    badLines <- lapply(inferredFiles, function(x) {
      which(
        (tolower(x) %in% projectFilesLower) &
        (!(x %in% projectFiles))
      )
    })
    
    Reduce(union, badLines)
    
  },
  
  takes = isRCodeFile,
  
  message = function(content, lines) {
    makeLinterMessage(
      "The following lines contain paths to files not matching in case sensitivity",
      content,
      lines
    )
  },
  
  suggestion = "Filepaths are case-sensitive on ShinyApps.io."
  
))
