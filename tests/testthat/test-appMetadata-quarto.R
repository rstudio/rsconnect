
# quarto ------------------------------------------------------------------

fakeQuartoMetadata <- function(version, engines) {
  # See quarto-r/R/publish.R lines 396 and 113.
  metadata <- list()
  metadata$quarto_version <- version
  metadata$quarto_engines <- I(engines)
  return(metadata)
}

test_that("inferQuartoInfo correctly detects info when quarto is provided alone", {
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

test_that("inferQuartoInfo extracts info from metadata", {
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

test_that("inferQuartoInfo prefers using metadata over quarto inspect", {
  skip_if_no_quarto()

  metadata <- fakeQuartoMetadata(version = "99.9.9", engines = c("internal-combustion"))

  quartoInfo <- inferQuartoInfo(
    appDir = test_path("quarto-website-r"),
    appPrimaryDoc = NULL,
    metadata = metadata
  )
  expect_equal(quartoInfo$engines, I(c("internal-combustion")))
})

test_that("inferQuartoInfo returns NULL for non-quarto content", {
  skip_if_no_quarto()

  quartoInfo <- inferQuartoInfo(
    appDir = test_path("shinyapp-simple"),
    appPrimaryDoc = NULL,
    metadata = list()
  )
  expect_null(quartoInfo)
})

test_that("quartoInspect requires quarto", {
  local_mocked_bindings(quarto_path = function() NULL)
  expect_snapshot(error = TRUE, {
    quartoInspect()
  })
})

test_that("quartoInspect identifies on Quarto projects", {
  skip_if_no_quarto()

  inspect <- quartoInspect(test_path("quarto-website-r"))
  expect_true(all(c("quarto", "engines") %in% names(inspect)))

  inspect <- quartoInspect(test_path("quarto-proj-r-shiny"))
  expect_true(all(c("quarto", "engines") %in% names(inspect)))
})

test_that("quartoInspect identifies Quarto documents", {
  skip_if_no_quarto()

  inspect <- quartoInspect(
    appDir = test_path("quarto-doc-none"),
    appPrimaryDoc = "quarto-doc-none.qmd"
  )
  expect_true(all(c("quarto", "engines") %in% names(inspect)))
})

test_that("quartoInspect returns NULL on non-quarto Quarto content", {
  skip_if_no_quarto()

  inspect <- quartoInspect(test_path("shinyapp-simple"))
  expect_null(inspect)
})
