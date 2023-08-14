# appMetadata -------------------------------------------------------------

test_that("quarto affects mode inference", {
  dir <- local_temp_app(list("foo.Rmd" = ""))

  metadata <- appMetadata(dir, c("foo.Rmd"))
  expect_equal(metadata$appMode, "rmd-static")

  metadata <- appMetadata(dir, c("foo.Rmd"), metadata = list(quarto_version = 1))
  expect_equal(metadata$appMode, "quarto-static")
})

test_that("quarto path is deprecated", {
  skip_if_no_quarto()
  dir <- local_temp_app(list("foo.Rmd" = ""))
  expect_snapshot(. <- appMetadata(dir, c("foo.Rmd"), quarto = "abc"))
})

test_that("validates quarto argument", {
  dir <- local_temp_app(list("foo.Rmd" = ""))
  expect_snapshot(appMetadata(dir, c("foo.Rmd"), quarto = 1), error = TRUE)
})


test_that("handles special case of appPrimaryDoc as R file", {
  dir <- local_temp_app(list("foo.R" = ""))
  metadata <- appMetadata(dir, c("foo.R"), appPrimaryDoc = "foo.R")
  expect_equal(metadata$appMode, "shiny")
})

# https://github.com/rstudio/rsconnect/issues/942
test_that("files beneath the root are not ignored when determining app-mode", {
  dir <- local_temp_app(list("app.R" = "", "plumber/api/plumber.R" = ""))
  metadata <- appMetadata(dir, c("app.R", "plumber/api/plumber.R"))
  expect_equal(metadata$appMode, "shiny")
})

test_that("content type (appMode) is inferred and can be overridden", {
  dir <- local_temp_app(list(
    "app.R" = "",
    "plumber.R" = "",
    "report.Rmd" = "",
    "index.html" = ""
  ))
  files <- c("app.R", "plumber.R", "report.Rmd", "index.html")

  metadata <- appMetadata(dir, files)
  expect_equal(metadata$appMode, "api")

  metadata <- appMetadata(dir, files, appMode = "shiny")
  expect_equal(metadata$appMode, "shiny")

  metadata <- appMetadata(dir, files, appMode = "rmd-static")
  expect_equal(metadata$appMode, "rmd-static")

  metadata <- appMetadata(dir, files, appMode = "static")
  expect_equal(metadata$appMode, "static")
})

# checkLayout -------------------------------------------------------------

test_that("checkLayout() errors if primary doc & app.R", {
  dir <- local_temp_app(list(
    "app.R" = "",
    "myscript.R" = ""
  ))

  expect_snapshot(checkAppLayout(dir, "myscript.R"), error = TRUE)
})

test_that("checkLayout fails if no known structure", {
  dir <- local_temp_app(list(
    "data.txt" = "",
    "cats.csv" = ""
  ))

  expect_snapshot(checkAppLayout(dir), error = TRUE)
})

test_that("checkLayout succeeds with some common app structures", {
  rmd <- local_temp_app(list("foo.Rmd" = ""))
  expect_no_error(checkAppLayout(rmd))

  shiny1 <- local_temp_app(list("app.R" = ""))
  expect_no_error(checkAppLayout(rmd))

  shiny2 <- local_temp_app(list("server.R" = "", "ui.R" = ""))
  expect_no_error(checkAppLayout(rmd))

  static <- local_temp_app(list("foo.html" = ""))
  expect_no_error(checkAppLayout(rmd))
})

# inferAppMode ------------------------------------------------------------

test_that("can infer mode for APIs", {
  expect_equal(inferAppMode("plumber.R"), "api")
  expect_equal(inferAppMode("entrypoint.R"), "api")
})

test_that("can infer mode for shiny apps", {
  expect_equal(inferAppMode("app.R"), "shiny")
  expect_equal(inferAppMode("server.R"), "shiny")
})

test_that("can infer mode for static quarto and rmd docs", {
  dir <- local_temp_app(list("foo.Rmd" = ""))
  paths <- list.files(dir, full.names = TRUE)

  expect_equal(inferAppMode(paths), "rmd-static")
  expect_equal(inferAppMode(paths, usesQuarto = TRUE), "quarto-static")
  # Static R Markdown treated as rmd-shiny for shinyapps targets
  expect_equal(inferAppMode(paths, isShinyappsServer = TRUE), "rmd-shiny")
})

test_that("can infer mode for shiny rmd docs", {
  yaml_runtime <- function(runtime) {
    c("---", paste0("runtime: ", runtime), "---")
  }

  dir <- local_temp_app(list("index.Rmd" = yaml_runtime("shiny")))
  paths <- list.files(dir, full.names = TRUE)
  expect_equal(inferAppMode(paths), "rmd-shiny")

  dir <- local_temp_app(list("index.Rmd" = yaml_runtime("shinyrmd")))
  paths <- list.files(dir, full.names = TRUE)
  expect_equal(inferAppMode(paths), "rmd-shiny")

  dir <- local_temp_app(list("index.Rmd" = yaml_runtime("shiny_prerendered")))
  paths <- list.files(dir, full.names = TRUE)
  expect_equal(inferAppMode(paths), "rmd-shiny")

  # can pair server.R with shiny runtime
  dir <- local_temp_app(list("index.Rmd" = yaml_runtime("shiny"), "server.R" = ""))
  paths <- list.files(dir, full.names = TRUE)
  expect_equal(inferAppMode(paths), "rmd-shiny")

  # Beats static rmarkdowns
  dir <- local_temp_app(list("index.Rmd" = yaml_runtime("shiny"), "foo.Rmd" = ""))
  paths <- list.files(dir, full.names = TRUE)
  expect_equal(inferAppMode(paths), "rmd-shiny")
})

test_that("can infer mode for shiny qmd docs", {
  yaml_runtime <- function(runtime) {
    c("---", paste0("runtime: ", runtime), "---")
  }

  dir <- local_temp_app(list("index.Qmd" = yaml_runtime("shiny")))
  paths <- list.files(dir, full.names = TRUE)
  expect_equal(inferAppMode(paths), "quarto-shiny")

  # Can force Rmd to use quarto
  dir <- local_temp_app(list("index.Rmd" = yaml_runtime("shiny")))
  paths <- list.files(dir, full.names = TRUE)
  expect_equal(inferAppMode(paths, usesQuarto = TRUE), "quarto-shiny")

  # Prefers quarto if both present
  dir <- local_temp_app(list(
    "index.Qmd" = yaml_runtime("shiny"),
    "index.Rmd" = yaml_runtime("shiny")
  ))
  paths <- list.files(dir, full.names = TRUE)
  expect_equal(inferAppMode(paths), "quarto-shiny")
})

test_that("Shiny R Markdown files are detected correctly", {
  expect_true(isShinyRmd(test_path("shiny-rmds/shiny-rmd-dashes.Rmd")))
  expect_true(isShinyRmd(test_path("shiny-rmds/shiny-rmd-dots.Rmd")))
  expect_false(isShinyRmd(test_path("shiny-rmds/non-shiny-rmd.Rmd")))
})

test_that("shiny metadata process correctly", {
  expect_false(is_shiny_prerendered(NULL, NULL))
  expect_true(is_shiny_prerendered("shiny_prerendered", NULL))
  expect_true(is_shiny_prerendered("shinyrmd", NULL))
  expect_true(is_shiny_prerendered(NULL, "shiny"))
  expect_true(is_shiny_prerendered(NULL, list(type = "shiny")))
})

test_that("otherwise, fallsback to static deploy", {
  expect_equal(inferAppMode(c("a.html", "b.html")), "static")
})

# inferAppPrimaryDoc ------------------------------------------------------

test_that("leaves addPrimaryDoc unchanged or not a document", {
  expect_equal(inferAppPrimaryDoc("foo.Rmd"), "foo.Rmd")
  expect_equal(inferAppPrimaryDoc(NULL, appMode = "shiny"), NULL)
  expect_equal(inferAppPrimaryDoc(NULL, appMode = "api"), NULL)
})

test_that("uses index file if present", {
  files <- c("index.html", "index.Rmd", "a.html", "b.html", "a.Rmd", "b.Rmd")
  expect_equal(inferAppPrimaryDoc(NULL, files, "static"), "index.html")
  expect_equal(inferAppPrimaryDoc(NULL, files, "rmd-shiny"), "index.Rmd")
})

test_that("otherwise fails back to first file with matching extensions", {
  files <- c("a.html", "b.html", "a.Rmd", "b.Rmd")
  expect_equal(inferAppPrimaryDoc(NULL, files, "static"), "a.html")
  expect_equal(inferAppPrimaryDoc(NULL, files, "rmd-shiny"), "a.Rmd")
})

test_that("errors if no files with needed extension", {
  expect_snapshot(error = TRUE, {
    inferAppPrimaryDoc(NULL, "a.R", "static")
    inferAppPrimaryDoc(NULL, "a.R", "rmd-shiny")
  })
})

# appHasParameters --------------------------------------------------------

test_that("non-documents don't have parameters", {
  dir <- local_temp_app(list("foo.R" = ""))

  expect_false(appHasParameters(dir, "foo.R", "static"))
  expect_false(appHasParameters(dir, "foo.R", "shiny"))
})

test_that("documents don't have parameters if part of a site", {
  dir <- local_temp_app(list("index.Rmd" = c("---", "params: [1, 2]", "---")))

  expect_false(appHasParameters(dir, "index.Rmd", "rmd-static", "site"))
  expect_false(appHasParameters(dir, "index.Rmd", "qmd-shiny", "site"))
})

test_that("non-Rmd files don't have parameters", {
  dir <- local_temp_app(list("app.r" = c("")))
  expect_false(appHasParameters(dir, "app.R", "rmd-shiny"))
})

test_that("otherwise look at yaml metadata", {
  dir <- local_temp_app(list("index.Rmd" = c("---", "params: [1, 2]", "---")))
  expect_true(appHasParameters(dir, "index.Rmd", "rmd-shiny"))

  dir <- local_temp_app(list("index.Rmd" = c("---", "params: ~", "---")))
  expect_false(appHasParameters(dir, "index.Rmd", "rmd-shiny"))
})

# detectPythonInDocuments -------------------------------------------------

test_that("dir without Rmds doesn't have have python", {
  dir <- local_temp_app()
  expect_false(detectPythonInDocuments(dir))

  dir <- local_temp_app(list("foo.R" = ""))
  expect_false(detectPythonInDocuments(dir))
})

test_that("Rmd or qmd with python chunk has python", {
  dir <- local_temp_app(list("foo.qmd" = c("```{r}", "1+1", "````")))
  expect_false(detectPythonInDocuments(dir))

  dir <- local_temp_app(list("foo.Rmd" = c("```{python}", "1+1", "````")))
  expect_true(detectPythonInDocuments(dir))

  dir <- local_temp_app(list("foo.qmd" = c("```{python}", "1+1", "````")))
  expect_true(detectPythonInDocuments(dir))
})
