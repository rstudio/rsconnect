# linter warns about absolute paths and relative paths

    Code
      result
    Output
      ---------------------
      ShinyPresentation.Rmd
      ---------------------
      The following lines contain absolute paths:
      15: Here's some internal help: [Helpful Link](/Users/MrBurns/)
      
      The following lines contain invalid relative paths (resolved outside of project directory):
      16: And another: [Favourite Goats](../../goats.txt)
      
      --------
      server.R
      --------
      The following lines contain absolute paths:
      15:     otherFile <- read.table("~/.rsconnect-tests/local-file.txt")
      
      The following lines contain paths to files not matching in case sensitivity:
      31:     file <- read.csv("college.txt") ## bad    ['college.txt' -> 'College.txt']
      
      The following lines contain invalid relative paths (resolved outside of project directory):
      16:     anotherFile <- readLines("../../foo.bar")
      
      Paths should be to files within the project directory.
      Filepaths are case-sensitive on deployment server.

