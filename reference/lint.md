# Lint a Project

Takes the set of active linters (see
[`addLinter()`](https://rstudio.github.io/rsconnect/reference/addLinter.md)),
and applies them to all files within a project.

Supported servers: All servers

## Usage

``` r
lint(project, files = NULL, appPrimaryDoc = NULL)
```

## Arguments

- project:

  Path to a project directory.

- files:

  Specific files to lint. Can be NULL, in which case all the files in
  the directory will be linted.

- appPrimaryDoc:

  The primary file in the project directory. Can be NULL, in which case
  it's inferred (if possible) from the directory contents.
