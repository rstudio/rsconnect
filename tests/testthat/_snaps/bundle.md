# writeManifest: Deploying a Quarto project without Quarto info in an error

    Code
      makeManifest(appDir, appPrimaryDoc = NULL, quarto = NULL)
    Condition
      Error in `inferAppMode()`:
      ! Can't deploy Quarto content when `quarto` is `NULL`.
      i Please supply a path to a quarto binary in `quarto`.

# writeManifest: Deploying a Quarto doc without Quarto info in an error

    Code
      makeManifest(appDir, appPrimaryDoc = appPrimaryDoc, quarto = NULL)
    Condition
      Error in `inferAppMode()`:
      ! Can't deploy Quarto content when `quarto` is `NULL`.
      i Please supply a path to a quarto binary in `quarto`.

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

