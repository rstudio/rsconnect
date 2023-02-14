# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static", assetTypeName = "asset")
    Condition
      Warning in `FUN()`:
      Package 'doesntexist1' not available in repository or locally
      Warning in `FUN()`:
      Package 'doesntexist2' not available in repository or locally
      Error:
      ! Unable to retrieve package records for the following packages:
      - 'doesntexist1', 'doesntexist2'

