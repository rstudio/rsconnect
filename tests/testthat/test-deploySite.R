test_that("can extract quarto metadata", {
  skip_on_cran()
  skip_if_no_quarto()
  skip_if_not_installed("quarto")

  app <- local_temp_app(list(`_quarto.yaml` = c(
    "project:",
    "  type: website",
    "",
    "website:",
    "  title: 'website-quarto'"
  )))

  site <- quartoSite(app)
  expect_equal(site$title, "website-quarto")
  expect_equal(site$output_dir, normalizePath(file.path(app, "_site")))
})

test_that("can extract rmarkdown metadata", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  app <- local_temp_app(list(
    `_site.yml` = 'name: "my-website"'
  ))

  site <- rmarkdownSite(app)
  expect_equal(site$name, "my-website")
  expect_equal(site$title, NULL)
  expect_equal(site$output_dir, normalizePath(file.path(app, "_site")))
})
