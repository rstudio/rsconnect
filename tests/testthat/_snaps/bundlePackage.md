# can snapshot deps with renv

    Code
      pkgs <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies
      v Found 2 dependencies

# can snapshot deps with packrat (option)

    Code
      pkgs <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies with packrat
      v Found 2 dependencies

# can snapshot deps with packrat (env var)

    Code
      pkgs <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies with packrat
      v Found 2 dependencies

# can capture deps from renv lockfile

    Code
      pkgs <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies from renv.lock
      v Found 3 dependencies

# can capture deps with packrat even when renv lockfile present

    Code
      pkgs <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies with packrat
      v Found 2 dependencies

# error if can't find source

    Code
      . <- bundlePackages(app_dir)
    Message
      i Capturing R dependencies
      v Found 1 dependency
    Condition
      Error:
      ! All packages must be installed from a reproducible location.
      x Can't re-install packages installed from source: shiny.
      i See `rsconnect::appDependencies()` for more details.

