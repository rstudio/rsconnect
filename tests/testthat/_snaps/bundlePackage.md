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

