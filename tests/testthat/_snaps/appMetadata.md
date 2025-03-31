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

# Shiny Quarto without an appropriate engine is an error

    Code
      appMetadata(dir, files)
    Condition
      Error in `appMetadata()`:
      ! The Quarto document requires a server but does not use an executable engine.
      Consider including some executable code, specifying an engine, or removing the server configuration.

# errors if no files with needed extension

    Code
      inferAppPrimaryDoc(NULL, "a.R", "static")
    Condition
      Error in `inferAppPrimaryDoc()`:
      ! Failed to determine `appPrimaryDoc` for "static" content.
      x No files matching "\\.html?$".
    Code
      inferAppPrimaryDoc(NULL, "a.html", "rmd-static")
    Condition
      Error in `inferAppPrimaryDoc()`:
      ! Failed to determine `appPrimaryDoc` for "rmd-static" content.
      x No files matching "\\.rmd$".
    Code
      inferAppPrimaryDoc(NULL, "a.html", "rmd-shiny")
    Condition
      Error in `inferAppPrimaryDoc()`:
      ! Failed to determine `appPrimaryDoc` for "rmd-shiny" content.
      x No files matching "\\.rmd$".
    Code
      inferAppPrimaryDoc(NULL, "a.html", "quarto-static")
    Condition
      Error in `inferAppPrimaryDoc()`:
      ! Failed to determine `appPrimaryDoc` for "quarto-static" content.
      x No files matching "\\.(r|rmd|qmd)$".
    Code
      inferAppPrimaryDoc(NULL, "a.html", "quarto-shiny")
    Condition
      Error in `inferAppPrimaryDoc()`:
      ! Failed to determine `appPrimaryDoc` for "quarto-shiny" content.
      x No files matching "\\.(rmd|qmd)$".

