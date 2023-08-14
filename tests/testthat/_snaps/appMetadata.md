# quarto path is deprecated

    Code
      . <- appMetadata(dir, c("foo.Rmd"), quarto = "abc")
    Condition
      Warning:
      The `quarto` argument of `deployApp()` can no longer be a path as of rsconnect 1.0.0.
      i Please use quarto = `TRUE` instead instead.

# validates quarto argument

    Code
      appMetadata(dir, c("foo.Rmd"), quarto = 1)
    Condition
      Error in `appMetadata()`:
      ! `quarto` must be `TRUE`, `FALSE`, or `NA`, not the number 1.

# checkLayout() errors if primary doc & app.R

    Code
      checkAppLayout(dir, "myscript.R")
    Condition
      Error in `checkAppLayout()`:
      ! Project must not contain both 'app.R' and a single-file Shiny app.

# checkLayout fails if no known structure

    Code
      checkAppLayout(dir)
    Condition
      Error in `checkAppLayout()`:
      ! Cancelling deployment: invalid project layout.
      i Expecting one of the following publication types:
        1. A Shiny app with `app.R` or `server.R` + `ui.R`
        2. R Markdown (`.Rmd`) or Quarto (`.qmd`) documents.
        3. A website containing `.html` and/or `.pdf` files.
        4. A plumber API with `plumber.R` or `entrypoint.R`.

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

