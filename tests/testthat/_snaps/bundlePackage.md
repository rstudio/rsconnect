# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static")
    Condition
      Error:
      ! All packages used by the asset must be installed.
      x Missing packages: doesntexist1 and doesntexist2.

# warns if can't find source

    Code
      . <- bundlePackages(app_dir, appMode = "rmd-static")
    Condition
      Warning:
      Local packages must be installed from a supported source.
      x Unsupported packages: shiny.
      i Supported sources are CRAN and CRAN-like repositories, BioConductor, GitHub, GitLab, and Bitbucket.
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

