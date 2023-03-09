# Deploying a Quarto project without Quarto info in an error

    Code
      makeManifest(appDir, quarto = NULL)
    Condition
      Error in `inferAppMode()`:
      ! Can't deploy Quarto content when `quarto` is `NULL`.
      i Please supply a path to a quarto binary in `quarto`.

# Deploying a Quarto doc without Quarto info in an error

    Code
      makeManifest(appDir, appPrimaryDoc = appPrimaryDoc)
    Condition
      Error in `inferAppMode()`:
      ! Can't deploy Quarto content when `quarto` is `NULL`.
      i Please supply a path to a quarto binary in `quarto`.

