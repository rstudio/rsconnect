# removes renv/packrat activation

    Code
      tweakRProfile(path)
      writeLines(readLines(path))
    Output
      # Modified by rsconnect package 0.8.29.1 on <NOW>
      # Line 1
      # renv initialization disabled in published application
      # source("renv/activate.R")
      # Line 3
      # Packrat initialization disabled in published application
      # source("packrat/init.R")
      # Line 5

