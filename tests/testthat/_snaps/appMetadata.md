# clear error if no files

    Code
      appMetadata(dir)
    Condition
      Error in `inferAppMode()`:
      ! No content to deploy; `appFiles` is empty

# quarto docs require quarto

    Code
      inferAppMode(single_qmd)
    Condition
      Error in `inferAppMode()`:
      ! Attempting to deploy Quarto content without Quarto metadata. Please provide the path to a quarto binary to the 'quarto' argument.
    Code
      inferAppMode(rmd_and_quarto_yml)
    Condition
      Error in `inferAppMode()`:
      ! Attempting to deploy Quarto content without Quarto metadata. Please provide the path to a quarto binary to the 'quarto' argument.

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

