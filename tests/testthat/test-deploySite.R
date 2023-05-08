test_that("can extract quarto metadata", {
  dir <- local_temp_app(`_quarto.yaml` = c(
    "project:",
    "  type: website",
    "",
    "website:",
    "  title: 'website-quarto'"
  ))

  site <- quartoSite(dir)
  expect_equal(site$title, "website-quarto")
  expect_equal(site$output_dir, normalizePath(file.path(dir, "_site")))
})

test_that("can extract rmarkdown metadata", {
  dir <- local_temp_app(
    `_site.yml` = 'name: "my-website"'
  )

  site <- rmarkdownSite(dir)
  expect_equal(site$name, "my-website")
  expect_equal(site$title, NULL)
  expect_equal(site$output_dir, normalizePath(file.path(dir, "_site")))
})
