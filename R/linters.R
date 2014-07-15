addLinter("absolute.paths", linter(

    apply = function(content, ...) {
      content <- stripComments(content)
      which(hasAbsolutePaths(content))
    },
    
    takes = function(paths) {
      grep("[rR]$", paths)
    },
    
    message = function(content, lines) {
      makeLinterMessage("The following lines contain absolute paths",
                        content,
                        lines)
    }
  
))

addLinter("invalid.relative.paths", linter(

  apply = function(content, ...) {
    content <- stripComments(content)
    badRelativePaths(content, ...)
  },
  
  takes = function(paths) {
    grep("[rR]$", paths, value = TRUE)
  },
  
  message = function(content, lines) {
    makeLinterMessage("The following lines contain invalid relative paths (resolved outside of project directory)",
                      content,
                      lines)
  }
  
))
