# errors if dependencies aren't installed

    Code
      bundlePackages(app_dir, appMode = "rmd-static")
    Condition
      Error:
      ! All packages used by the asset must be installed.
      x Missing packages: doesntexist1 and doesntexist2.

# warns if can't find source

    Code
      . <- bundlePackages(app_dir, appMode = "rmd-static")
    Condition
      Warning:
      Local packages must be installed from a supported source.
      x Unsupported packages: shiny.
      i Supported sources are CRAN and CRAN-like repositories, BioConductor, GitHub, GitLab, and Bitbucket.
      i See `rsconnect::appDependencies()` for more details.

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

