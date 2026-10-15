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

  # Flags an assignment of an `if` expression that has a branch in braces. A
  # one-line `x <- if (a) b else c` is not flagged.
  if_assignment_linter <- function() {
    xpath <- "//*[LEFT_ASSIGN or EQ_ASSIGN]/expr[IF][expr[OP-LEFT-BRACE]]"
    lintr::Linter(linter_level = "expression", function(source_expression) {
      lintr::xml_nodes_to_lints(
        xml2::xml_find_all(source_expression$xml_parsed_content, xpath),
        source_expression = source_expression,
        lint_message = paste(
          "Do not assign the result of an `if` block with braces.",
          "Assign the variable inside the block."
        ),
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
    test_helper_linter = test_helper_linter(),
    if_assignment_linter = if_assignment_linter()
  )
})

exclusions <- list(
  "R/import-standalone-obj-type.R",
  "R/import-standalone-types-check.R"
)
