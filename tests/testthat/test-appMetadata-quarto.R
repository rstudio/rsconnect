
# quarto ------------------------------------------------------------------

fakeQuartoMetadata <- function(version, engines) {
  # See quarto-r/R/publish.R lines 396 and 113.
  metadata <- list()
  metadata$quarto_version <- version
  metadata$quarto_engines <- I(engines)
  return(metadata)
}

test_that("inferQuartoInfo correctly detects info when quarto is provided alone", {
  skip_on_cran()
  skip_if_no_quarto()

  quartoInfo <- inferQuartoInfo(
    metadata = list(),
    appDir = test_path("quarto-doc-none"),
    appPrimaryDoc = "quarto-doc-none.qmd"
  )
  expect_named(quartoInfo, c("version", "engines"))
  expect_equal(quartoInfo$engines, I(c("markdown")))

  quartoInfo <- inferQuartoInfo(
    appDir = test_path("quarto-website-r"),
    appPrimaryDoc = NULL,
    metadata = list()
  )
  expect_named(quartoInfo, c("version", "engines"))
  expect_equal(quartoInfo$engines, I(c("knitr")))
})

test_that("inferQuartoInfo prefers metadata over quarto inspect", {
  metadata <- fakeQuartoMetadata(version = "99.9.9", engines = c("internal-combustion"))

  quartoInfo <- inferQuartoInfo(
    appDir = test_path("quarto-website-r"),
    appPrimaryDoc = NULL,
    metadata = metadata
  )
  expect_equal(quartoInfo, list(
    version = "99.9.9",
    engines = I("internal-combustion")
  ))
})

test_that("quartoInspect requires quarto", {
  local_mocked_bindings(quarto_path = function() NULL)
  expect_snapshot(error = TRUE, {
    quartoInspect()
  })
})

test_that("quartoInspect identifies Quarto projects", {
  skip_on_cran()
  skip_if_no_quarto()

  inspect <- quartoInspect(test_path("quarto-website-r"))
  expect_true(all(c("quarto", "engines") %in% names(inspect)))

  inspect <- quartoInspect(test_path("quarto-proj-r-shiny"))
  expect_true(all(c("quarto", "engines") %in% names(inspect)))
})

test_that("quartoInspect identifies Quarto documents", {
  skip_on_cran()
  skip_if_no_quarto()

  inspect <- quartoInspect(
    appDir = test_path("quarto-doc-none"),
    appPrimaryDoc = "quarto-doc-none.qmd"
  )
  expect_true(all(c("quarto", "engines") %in% names(inspect)))
})

test_that("quartoInspect processes content within paths containing spaces", {
  skip_on_cran()
  skip_if_no_quarto()

  parent <- withr::local_tempdir()
  dir <- file.path(parent, "space dir")
  dir.create(dir)
  writeLines(c(
    "---",
    "title: space path",
    "---",
    "this is a document within a path having spaces."
  ), file.path(dir, "index.qmd"))
  inspect <- quartoInspect(dir, "index.qmd")
  expect_equal(inspect$engines, c("markdown"))
})

test_that("quartoInspect processes content with filenames containing spaces", {
  skip_on_cran()
  skip_if_no_quarto()

  dir <- local_temp_app(list("space file.qmd" = c(
    "---",
    "title: space name",
    "---",
    "this is a document with a filename having spaces."
  )))
  inspect <- quartoInspect(dir, "space file.qmd")
  expect_equal(inspect$engines, c("markdown"))
})

# Some versions of Quarto show Quarto stack traces with source references when
# inspect fails.
#
# Only preserve:
#
#   Error in `quartoInspect()`:
#   ! Failed to run `quarto inspect` against your content:
#   ERROR: Unsupported project type unsupported
strip_quarto_trace <- function(lines) {
  head(lines, 3)
}

test_that("quartoInspect produces an error when a document cannot be inspected", {
  skip_on_cran()
  skip_if_no_quarto()

  # Suppress colors from Quarto errors.
  withr::local_envvar(NO_COLOR = "1")

  dir <- local_temp_app(list("bad.qmd" = c(
    "---",
    "format: unsupported",
    "---",
    "this is a document using an unsupported format."
  )))
  expect_snapshot(
    quartoInspect(dir, "bad.qmd"),
    error = TRUE,
    transform = strip_quarto_trace
  )
})

test_that("quartoInspect produces an error when a project cannot be inspected", {
  skip_on_cran()
  skip_if_no_quarto()

  # Suppress colors from Quarto errors.
  withr::local_envvar(NO_COLOR = "1")

  dir <- local_temp_app(
    list(
      "_quarto.yml" = c(
        "project:",
        "  type: unsupported"
      ),
      "bad.qmd" = c(
        "---",
        "title: bad",
        "---",
        "this is a document using an unsupported format."
      )
    )
  )
  expect_snapshot(
    quartoInspect(dir, "bad.qmd"),
    error = TRUE,
    transform = strip_quarto_trace
  )
})
