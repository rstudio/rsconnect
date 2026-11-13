# removes renv/packrat activation

    Code
      tweakRProfile(path)
      writeLines(readLines(path))
    Output
      # Modified by rsconnect package <VERSION> on <NOW>
      # Line 1
      # renv initialization disabled in published application
      # source("renv/activate.R")
      # Line 3
      # Packrat initialization disabled in published application
      # source("packrat/init.R")
      # Line 5

# versionFromDescription() warns about two constraints in one R entry

    Code
      requires <- versionFromDescription(appDir)
    Condition
      Warning:
      Ignoring an R version requirement in 'DESCRIPTION' that is not valid.
      i Write each constraint as a separate entry, for example `R (>= 4.2.0), R (< 4.3)`.

