# Linter(linter_level =) needs lintr 3.2.0.
if (utils::packageVersion("lintr") < "3.2.0") {
  stop("The rsconnect lint config needs lintr 3.2.0 or later.", call. = FALSE)
}

linters <- local({
  # Flags objects that a test-*.R file assigns at the top level. Test helpers
  # and shared test data must go in a helper*.R file, so that
  # devtools::load_all() loads them.
  test_helper_linter <- function() {
    xpath <- "/exprlist/*[LEFT_ASSIGN or EQ_ASSIGN or RIGHT_ASSIGN]"
    lintr::Linter(linter_level = "file", function(source_expression) {
      if (!grepl("^test-.*\\.[Rr]$", basename(source_expression$filename))) {
        return(list())
      }
      lintr::xml_nodes_to_lints(
        xml2::xml_find_all(source_expression$full_xml_parsed_content, xpath),
        source_expression = source_expression,
        lint_message = "Define test helpers and shared test data in a helper*.R file, not in a test file.",
        type = "warning"
      )
    })
  }

  lintr::linters_with_defaults(
    lintr::line_length_linter(160),
    indentation_linter = NULL,
    lintr::object_length_linter(60),
    object_name_linter = NULL,
    object_usage_linter = NULL,
    return_linter = NULL,
    test_helper_linter = test_helper_linter()
  )
})

exclusions <- list(
  "R/import-standalone-obj-type.R",
  "R/import-standalone-types-check.R"
)
