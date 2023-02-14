# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static", assetTypeName = "asset")
    Condition
      Error:
      ! 
      * Asset depends on package "doesntexist1"but it is not installed.
        Please resolve before continuing.
      
      * Asset depends on package "doesntexist2"but it is not installed.
        Please resolve before continuing.

# warns if can't find source

    Code
      . <- bundlePackages(app_dir, appMode = "rmd-static", assetTypeName = "asset")
    Condition
      Warning:
      
      * May be unable to deploy package dependency 'shiny'; could not
        determine a repository URL for the source 'foo'.
      
      * Unable to determine the source location for some packages. Packages
        should be installed from a package repository like CRAN or a version
        control system. Check that options('repos') refers to a package
        repository containing the needed package versions.

