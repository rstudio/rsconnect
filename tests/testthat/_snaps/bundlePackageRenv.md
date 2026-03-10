# large directories are analyzed

    Code
      deps <- snapshotRenvDependencies(app_dir)

# errors if library and project are inconsistent

    Code
      parseRenvDependencies(file.path(app_dir, "renv.lock"), app_dir)
    Condition
      Error in `parseRenvDependencies()`:
      ! Library and lockfile are out of sync
      i Use renv::restore() or renv::snapshot() to synchronise
      i Or ignore the lockfile by adding to your .rscignore

