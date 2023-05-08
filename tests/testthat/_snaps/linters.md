# linter warns about absolute paths and relative paths

    Code
      result
    Output
      ---------------------
      ShinyPresentation.Rmd
      ---------------------
      The following lines contain absolute paths:
      15: Here's some internal help: [Helpful Link](/Users/MrBurns/)
      
      --------
      server.R
      --------
      The following lines contain absolute paths:
      15:     otherFile <- read.table("~/.rsconnect-tests/local-file.txt")
      
      The following lines contain paths to files not matching in case sensitivity:
      31:     file <- read.csv("data/college.txt") ## bad    ['data/college.txt' -> 'data/College.txt']
      
      Paths should be to files within the project directory.
      Filepaths are case-sensitive on deployment server.

