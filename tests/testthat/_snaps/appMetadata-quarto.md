# quartoInspect requires quarto

    Code
      quartoInspect()
    Condition
      Error in `quartoInspect()`:
      ! `quarto` not found.
      i Check that it is installed and available on your `PATH`.

# quartoInspect produces an error when a document cannot be inspected

    Code
      quartoInspect(dir, "bad.qmd")
    Condition
      Error in `quartoInspect()`:
      ! Failed to run `quarto inspect` against your content:
      [91mERROR: Unknown format unsupported[39m

# quartoInspect produces an error when a project cannot be inspected

    Code
      quartoInspect(dir, "bad.qmd")
    Condition
      Error in `quartoInspect()`:
      ! Failed to run `quarto inspect` against your content:
      [91mERROR: Unsupported project type unsupported[39m

