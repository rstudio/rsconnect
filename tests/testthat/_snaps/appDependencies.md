# infers correct packages for each source

    Code
      inferRPackageDependencies(simulateMetadata("rmd-static"))
    Output
      [1] "rmarkdown"
    Code
      inferRPackageDependencies(simulateMetadata("rmd-static", hasParameters = TRUE))
    Output
      [1] "rmarkdown" "shiny"    
    Code
      inferRPackageDependencies(simulateMetadata("quarto-static"))
    Output
      [1] "rmarkdown"
    Code
      inferRPackageDependencies(simulateMetadata("quarto-shiny"))
    Output
      [1] "rmarkdown" "shiny"    
    Code
      inferRPackageDependencies(simulateMetadata("rmd-shiny"))
    Output
      [1] "rmarkdown" "shiny"    
    Code
      inferRPackageDependencies(simulateMetadata("shiny"))
    Output
      [1] "shiny"
    Code
      inferRPackageDependencies(simulateMetadata("api"))
    Output
      [1] "plumber"
    Code
      inferRPackageDependencies(simulateMetadata("api", documentsHavePython = TRUE))
    Output
      [1] "plumber"    "reticulate"

