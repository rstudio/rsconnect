# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static", assetTypeName = "asset")
    Condition
      Error in `bundlePackages()`:
      ! All packages used by asset must be installed.
      x Missing packages: doesntexist1 and doesntexist2.

# warns if can't find source

    Code
      . <- bundlePackages(app_dir, appMode = "rmd-static", assetTypeName = "asset")
    Condition
      Warning:
      Packages require a known repository for install on remote system.
      x Packages with unknown repository: shiny.
      i Automatic source detection relies on packages being installed from a standard repository like CRAN or BioConductor, or from a version control system like GitHub or GitLab.

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

