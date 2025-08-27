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

quarto_project_r_shiny_files <- list(
  "_quarto.yml" = c(
    "project:",
    '  title: "quarto-proj-r-shiny"'
  ),

  "quarto-proj-r-shiny.qmd" = c(
    "---",
    'title: "quarto-proj-r-shiny"',
    "resource_files:",
    "- _quarto.yml",
    "format: ",
    "  html:",
    "    page-layout: custom",
    "server: shiny",
    "---",
    "",
    "```{r}",
    "#| panel: sidebar",
    "vars <- setdiff(names(iris), 'Species')",
    "selectInput('xcol', 'X Variable', vars)",
    "selectInput('ycol', 'Y Variable', vars, selected = vars[[2]])",
    "numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)",
    "```",
    "",
    "```{r}",
    "#| panel: fill",
    "plotOutput('plot1')",
    "```",
    "",
    "```{r}",
    "#| context: server",
    "selectedData <- reactive({",
    "    iris[, c(input$xcol, input$ycol)]",
    "  })",
    "",
    "clusters <- reactive({",
    "  kmeans(selectedData(), input$clusters)",
    "})",
    "",
    "output$plot1 <- renderPlot({",
    "  palette(c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3',",
    "    '#FF7F00', '#FFFF33', '#A65628', '#F781BF', '#999999'))",
    "",
    "  par(mar = c(5.1, 4.1, 0, 1))",
    "  plot(selectedData(),",
    "       col = clusters()$cluster,",
    "       pch = 20, cex = 3)",
    "  points(clusters()$centers, pch = 4, cex = 4, lwd = 4)",
    "})",
    "```"
  )
)
