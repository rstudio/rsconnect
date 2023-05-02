# lints give have useful print method

    Code
      lint(test_path("test-rmd-bad-case"))
    Output
      ---------
      index.Rmd
      ---------
      The following lines contain paths to files not matching in case sensitivity:
      29: ![](img/rstudio.svg)    ['img/rstudio.svg' -> 'img/RStudio.svg']
      
      Filepaths are case-sensitive on deployment server.
    Code
      lint(test_path("shinyapp-appR"))
    Output
      No problems found

