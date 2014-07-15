## A linter is a list with components:
## apply: A function that takes some content and returns indices that are 'bad',
## takes: A function determining what paths it will attempt to lint
## message:  A function that produces an appropriate message based on lint results

addLinter("absolute.paths", linter(

    apply = function(content, ...) {
      content <- stripComments(content)
      which(hasAbsolutePaths(content))
    },
    
    takes = function(paths) {
      grep("[rR]$", paths, value = TRUE)
    },
    
    message = function(content, lines) {
      makeLinterMessage("The following lines contain absolute paths:",
                        content,
                        lines)
    }
  
))

# addLinter("invalid.relative.paths", linter(
# 
#   apply = function(content, ...) {
#     content <- stripComments(content)
#     which(badRelativePath(content))
#   },
#   
#   takes = function(paths) {
#     grep("[rR]$", paths, value = TRUE)
#   },
#   
#   message = function(content, lines) {
#     makeLinterMessage("The following lines contain invalid relative paths",
#                       content,
#                       lines)
#   }
#   
# ))