# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static")
    Message
      i Capturing R dependencies with packrat
      v Found 2 dependencies.
    Condition
      Error:
      ! All packages used by the asset must be installed.
      x Missing packages: doesntexist1 and doesntexist2.

# warns if can't find source

    Code
      . <- bundlePackages(app_dir, appMode = "rmd-static")
    Message
      i Capturing R dependencies with packrat
      v Found 1 dependency.
    Condition
      Error:
      ! All packages must be installed from a reproducible location.
      x Can't re-install packages installed from source: shiny.
      i See `rsconnect::appDependencies()` for more details.

# clear error if can't run performPackratSnapshot()

    Code
      addPackratSnapshot(dir, "doesntexist")
    Condition
      Warning in `FUN()`:
      Package 'doesntexist' not available in repository or locally
      Error in `addPackratSnapshot()`:
      ! Failed to snapshot dependencies
      Caused by error:
      ! Unable to retrieve package records for the following packages:
      - 'doesntexist'

