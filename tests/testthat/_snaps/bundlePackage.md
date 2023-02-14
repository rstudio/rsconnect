# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static", assetTypeName = "asset")
    Condition
      Error:
      ! 
      * Asset depends on package 'doesntexist1'but it is not installed.
        Please resolve before continuing.
      
      * Asset depends on package 'doesntexist2'but it is not installed.
        Please resolve before continuing.

