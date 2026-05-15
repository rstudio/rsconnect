# large directories are analyzed

    Code
      deps <- snapshotRenvDependencies(app_dir)

# errors when renv::snapshot fails

    Code
      snapshotRenvDependencies(app_dir)
    Condition
      Error in `snapshotRenvDependencies()`:
      ! Failed to snapshot dependencies with renv.
      i For example, you have a locally-developed package that is installed from disk.
      Caused by error in `renv::snapshot()`:
      ! can't snapshot this package

# errors if library and project are inconsistent

    Code
      parseRenvDependencies(file.path(app_dir, "renv.lock"), app_dir)
    Condition
      Error in `parseRenvDependencies()`:
      ! Library and lockfile are out of sync
      i Use renv::restore() or renv::snapshot() to synchronise
      i Or ignore the lockfile by adding to your .rscignore
      i Or set `dependencyResolution = "library"` to ignore the lockfile and use the local library instead

