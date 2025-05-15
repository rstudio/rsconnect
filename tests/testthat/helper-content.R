# A basic Quarto website that uses Python.
quarto_website_py_files <- list(
  "_quarto.yml" = c(
    "project:",
    "  type: website",
    "",
    "website:",
    "  title: quarto-website-py",
    "  navbar:",
    "    background: primary",
    "    left:",
    "      - href: index.qmd",
    "        text: Home",
    "      - about.qmd",
    "",
    "format:",
    "  html:",
    "    theme: cosmo",
    "    css: styles.css",
    "",
    "editor: visual"
  ),
  index.qmd = c(
    "---",
    "title: quarto-website-py",
    "jupyter: python3",
    "---",
    "",
    "This is a Quarto website.",
    "",
    "To learn more about Quarto websites visit <https://quarto.org/docs/websites>.",
    "",
    "```{python}",
    "1 + 1",
    "```"
  ),
  requirements.txt = "jupyter",
  styles.css = "/* css styles */"
)

# A basic Quarto website that uses Python and has a .python-version file.
quarto_website_py_python_version_files <- utils::modifyList(
  quarto_website_py_files,
  list(
    ".python-version" = "3.8",
    about.qmd = c(
      "---",
      "title: About",
      "jupyter: python3",
      "---",
      "",
      "About this site",
      "",
      "```{python}",
      "1 + 1",
      "```"
    )
  )
)

# A basic Quarto website that uses Python and has a setup.cfg file.
quarto_website_py_setup_cfg_files <- utils::modifyList(
  quarto_website_py_files,
  list(
    setup.cfg = c(
      "[metadata]",
      "name = a-quarto-project",
      "version = 0.1.0",
      "description = Add your description here",
      "",
      "[options]",
      "python_requires = >=3.9"
    )
  )
)

quarto_website_r_files <- list(
  "_quarto.yml" = c(
    "project:",
    "  type: website",
    "",
    "website:",
    "  title: quarto-website-r",
    "  navbar:",
    "    background: primary",
    "    left:",
    "      - href: index.qmd",
    "        text: Home",
    "      - about.qmd",
    "",
    "format:",
    "  html:",
    "    theme: cosmo",
    "    css: styles.css",
    "",
    "editor: visual"
  ),
  about.qmd = c(
    "---",
    "title: About",
    "---",
    "",
    "About this site",
    "",
    "```{r}",
    "1 + 1",
    "```    "
  ),
  index.qmd = c(
    "---",
    "title: quarto-website-r",
    "---",
    "",
    "This is a Quarto website.",
    "",
    "To learn more about Quarto websites visit <https://quarto.org/docs/websites>.",
    "",
    "```{r}",
    "1 + 1",
    "```"
  ),
  styles.css = "/* css styles */"
)

quarto_website_r_py_files <- list(
  "_quarto.yml" = c(
    "project:",
    "  type: website",
    "",
    "website:",
    "  title: quarto-website-r-py",
    "  navbar:",
    "    background: primary",
    "    left:",
    "      - href: index.qmd",
    "        text: Home",
    "      - about.qmd",
    "",
    "format:",
    "  html:",
    "    theme: cosmo",
    "    css: styles.css",
    "",
    "editor: visual"
  ),
  about.qmd = c(
    "---",
    "title: About",
    "---",
    "",
    "About this site",
    "",
    "```{r}",
    "# This is an R chunk",
    "1 + 1",
    "```",
    "",
    "```{python}",
    "# This is a Python chunk",
    "1 + 1",
    "```"
  ),
  index.qmd = c(
    "---",
    "title: quarto-website-r-py",
    "---",
    "",
    "This is a Quarto website.",
    "",
    "To learn more about Quarto websites visit <https://quarto.org/docs/websites>.",
    "",
    "```{r}",
    "# This is an R chunk",
    "1 + 1",
    "```",
    "",
    "```{python}",
    "# This is a Python chunk",
    "1 + 1",
    "```"
  ),
  pyproject.toml = c(
    "[project]",
    'name = "a-quarto-project"',
    'version = "0.1.0"',
    'description = "Add your description here"',
    'requires-python = ">=3.11"'
  ),
  requirements.txt = c(
    "# stub requirements.txt",
    "# Its presence indicates the need for Python dependencies."
  ),
  styles.css = "/* css styles */"
)
