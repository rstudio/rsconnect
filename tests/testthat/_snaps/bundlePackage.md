# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static")
    Condition
      Error:
      ! * Deployment depends on package "doesntexist1"but it is not installed.
        Please resolve before continuing.
      
      * Deployment depends on package "doesntexist2"but it is not installed.
        Please resolve before continuing.

# warns if can't find source

    Code
      . <- bundlePackages(app_dir, appMode = "rmd-static")
    Condition
      Warning:
      * May be unable to deploy package dependency 'shiny'; could not
        determine a repository URL for the source 'foo'.
      
      * Unable to determine the source location for some packages. Packages
        should be installed from a package repository like CRAN or a version
        control system. Check that options('repos') refers to a package
        repository containing the needed package versions.

# infers correct packages for each source

    Code
      inferRPackageDependencies("rmd-static")
    Output
      [1] "rmarkdown"
    Code
      inferRPackageDependencies("rmd-static", TRUE)
    Output
      [1] "rmarkdown" "shiny"    
    Code
      inferRPackageDependencies("quarto-static")
    Output
      [1] "rmarkdown"
    Code
      inferRPackageDependencies("quarto-shiny")
    Output
      [1] "rmarkdown" "shiny"    
    Code
      inferRPackageDependencies("rmd-shiny")
    Output
      [1] "rmarkdown" "shiny"    
    Code
      inferRPackageDependencies("shiny")
    Output
      [1] "shiny"
    Code
      inferRPackageDependencies("api")
    Output
      [1] "plumber"
    Code
      inferRPackageDependencies("api", documentsHavePython = TRUE)
    Output
      [1] "plumber"    "reticulate"

