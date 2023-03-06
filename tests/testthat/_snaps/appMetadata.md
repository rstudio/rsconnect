# clear error if no files

    Code
      appMetadata(dir)
    Condition
      Error in `appMetadata()`:
      ! No content to deploy.
      x `appDir` is empty.

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

# quarto docs require quarto

    Code
      inferAppMode(single_qmd)
    Condition
      Error in `inferAppMode()`:
      ! Can't deploy Quarto content when `quarto` is `NULL`.
      i Please supply a path to a quarto binary in `quarto`.
    Code
      inferAppMode(rmd_and_quarto_yml)
    Condition
      Error in `inferAppMode()`:
      ! Can't deploy Quarto content when `quarto` is `NULL`.
      i Please supply a path to a quarto binary in `quarto`.

# errors if no files with needed extension

    Code
      inferAppPrimaryDoc(NULL, "a.R", "static")
    Condition
      Error in `inferAppPrimaryDoc()`:
      ! Failed to determine `appPrimaryDoc`.
      x No files matching "\\.html?$".
    Code
      inferAppPrimaryDoc(NULL, "a.R", "rmd-shiny")
    Condition
      Error in `inferAppPrimaryDoc()`:
      ! Failed to determine `appPrimaryDoc`.
      x No files matching "\\.[Rq]md$".

