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
      checkAppLayout(dir, appPrimaryDoc = "myscript.R")
    Condition
      Error in `checkAppLayout()`:
      ! Project must not contain both 'app.R' and a single-file Shiny app.

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

