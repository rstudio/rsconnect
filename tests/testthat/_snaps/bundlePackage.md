# can snapshot deps with renv

    Code
      pkgs <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies with renv
      v Found 1 dependency

# can snapshot deps with packrat

    Code
      pkgs <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies with packrat
      v Found 1 dependency

# can capture deps from renv lockfile

    Code
      pkgs <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies from renv.lock
      v Found 1 dependency

# error if can't find source

    Code
      . <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies with renv
      v Found 1 dependency
    Condition
      Error:
      ! All packages must be installed from a reproducible location.
      x Can't re-install packages installed from source: shiny.
      i See `rsconnect::appDependencies()` for more details.

