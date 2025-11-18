# Add a Linter

Add a linter, to be used in subsequent calls to
[`lint()`](https://rstudio.github.io/rsconnect/reference/lint.md).

Supported servers: All servers

Add a linter, to be used in subsequent calls to
[`lint()`](https://rstudio.github.io/rsconnect/reference/lint.md).

## Usage

``` r
addLinter(name, linter)

addLinter(name, linter)
```

## Arguments

- name:

  The name of the linter, as a string.

- linter:

  A
  [`linter()`](https://rstudio.github.io/rsconnect/reference/linter.md).

## Examples

``` r
addLinter("no.capitals", linter(

  ## Identify lines containing capital letters -- either by name or by index
  apply = function(content, ...) {
    grep("[A-Z]", content)
  },

  ## Only use this linter on R files (paths ending with .r or .R)
  takes = function(paths) {
    grep("[rR]$", paths)
  },

  # Use the default message constructor
  message = function(content, lines, ...) {
    makeLinterMessage("Capital letters found on the following lines", content, lines)
  },

  # Give a suggested prescription
  suggest = "Do not use capital letters in these documents."
))
addLinter("no.capitals", linter(

  ## Identify lines containing capital letters -- either by name or by index
  apply = function(content, ...) {
    grep("[A-Z]", content)
  },

  ## Only use this linter on R files (paths ending with .r or .R)
  takes = function(paths) {
    grep("[rR]$", paths)
  },

  # Use the default message constructor
  message = function(content, lines, ...) {
    makeLinterMessage("Capital letters found on the following lines", content, lines)
  },

  # Give a suggested prescription
  suggest = "Do not use capital letters in these documents."
))
```
