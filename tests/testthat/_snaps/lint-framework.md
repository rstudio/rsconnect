# checkLayout() errors if primary doc & app.R

    Code
      checkAppLayout(dir, "myscript.R")
    Condition
      Error in `checkAppLayout()`:
      ! The project contains both a single-file Shiny application and a file named app.R; it must contain only one of these.

# checkLayout fails if no known structure

    Code
      checkAppLayout(dir)
    Condition
      Error in `checkAppLayout()`:
      ! Cancelling deployment: invalid project layout.
      The project should have one of the following layouts:
      1. 'server.R' and 'ui.R' in the application base directory,
      2. 'server.R' and 'www/index.html' in the application base directory,
      3. 'app.R' or a single-file Shiny .R file,
      4. An R Markdown (.Rmd) or Quarto (.qmd) document,
      5. A static HTML (.html) or PDF (.pdf) document.
      6. 'plumber.R' API description .R file
      7. 'entrypoint.R' plumber startup script
      8. A tensorflow saved model

