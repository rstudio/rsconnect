context("type detection")

test_that("Shiny R Markdown files are detected correctly", {
  expect_true(isShinyRmd("./shiny-rmds/shiny-rmd-dashes.Rmd"))
  expect_true(isShinyRmd("./shiny-rmds/shiny-rmd-dots.Rmd"))
  expect_false(isShinyRmd("./shiny-rmds/non-shiny-rmd.Rmd"))
})

# fileContent is a named list with file content. When content is NA, the
# name of the file is used as its content.
#
# Each file is written with its content to a temporary directory.
# Returns the result of inferAppMode when run against that directory.
# The temporary directory is removed on exit.
inferAppModeFromFiles <- function(fileContent, isCloudServer = FALSE) {
  targetDir <- tempfile()
  dir.create(targetDir)
  on.exit(unlink(targetDir, recursive = TRUE))

  for (file in names(fileContent)) {
    content <- fileContent[[file]]
    # Write the filename as its content when we are not given content.
    if (all(is.na(content))) {
      content <- c(file)
    }
    targetFile <- file.path(targetDir, file)
    writeLines(content, con = targetFile, sep = "\n")
  }

  files <- list.files(targetDir, recursive = FALSE, all.files = FALSE, include.dirs = FALSE, no.. = TRUE, full.names = FALSE)
  appMode <- inferAppMode(targetDir, NULL, files, quartoInfo = NULL, isCloudServer = isCloudServer)
  return(appMode)
}

test_that("inferAppMode", {
  # Plumber API identification
  expect_identical("api", inferAppModeFromFiles(list(
    "plumber.R" = NA
  )))
  expect_identical("api", inferAppModeFromFiles(list(
    "entrypoint.R" = NA
  )))
  expect_identical("api", inferAppModeFromFiles(list(
    "plumber.R" = NA,
    "helper.R" = NA
  )))

  # Shiny application identification
  expect_identical("shiny", inferAppModeFromFiles(list(
    "app.R" = NA
  )))
  expect_identical("shiny", inferAppModeFromFiles(list(
    "server.R" = NA
  )))
  expect_identical("shiny", inferAppModeFromFiles(list(
    "server.R" = NA,
    "ui.R" = NA
  )))
  expect_identical("shiny", inferAppModeFromFiles(list(
    "server.R" = NA,
    "ui.R" = NA,
    "global.R" = NA
  )))

  # Static R Markdown identification (rendered documents)
  expect_identical("rmd-static", inferAppModeFromFiles(list(
    "index.Rmd" = NA
  )))
  expect_identical("rmd-static", inferAppModeFromFiles(list(
    "index.Rmd" = NA,
    "alpha.Rmd" = NA,
    "bravo.Rmd" = NA
  )))
  expect_identical("rmd-static", inferAppModeFromFiles(list(
    "alpha.Rmd" = NA,
    "bravo.Rmd" = NA
  )))

  # Static R Markdown treated as rmd-shiny for shinyapps and rstudio.cloud targets
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "index.Rmd" = NA
  ), isCloudServer = TRUE))
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "index.Rmd" = NA,
    "alpha.Rmd" = NA,
    "bravo.Rmd" = NA
  ), isCloudServer = TRUE))
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "alpha.Rmd" = NA,
    "bravo.Rmd" = NA
  ), isCloudServer = TRUE))

  rmdRuntimeShiny <- c(
    "---",
    "runtime: shiny",
    "---"
  )
  rmdRuntimeShinyRmd <- c(
    "---",
    "runtime: shinyrmd",
    "---"
  )
  rmdRuntimeShinyPrerendered <- c(
    "---",
    "runtime: shiny_prerendered",
    "---"
  )
  # Shiny R Markdown identification
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "index.Rmd" = rmdRuntimeShiny
  )))
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "index.Rmd" = rmdRuntimeShinyRmd
  )))
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "index.Rmd" = rmdRuntimeShinyPrerendered
  )))
  # Shiny Rmd with other Rmd
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "shiny.Rmd" = rmdRuntimeShinyPrerendered,
    "other.Rmd" = NA
  )))
  # Shiny Rmd with other R script
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "shiny.Rmd" = rmdRuntimeShinyPrerendered,
    "helper.R" = NA
  )))
  # Shiny Rmd with server.R script
  expect_identical("rmd-shiny", inferAppModeFromFiles(list(
    "shiny.Rmd" = rmdRuntimeShinyPrerendered,
    "server.R" = NA
  )))
})
