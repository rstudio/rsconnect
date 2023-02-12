# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static", assetTypeName = "asset")
    Warning <simpleWarning>
      Package 'doesntexist1' not available in repository or locally
      Package 'doesntexist2' not available in repository or locally
    Error <simpleError>
      Unable to retrieve package records for the following packages:
      - 'doesntexist1', 'doesntexist2'

