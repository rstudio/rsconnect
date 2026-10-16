# AGENTS.md

Read
[CONTRIBUTING.md](https://rstudio.github.io/rsconnect/dev/CONTRIBUTING.md)
before you change code. It contains the commands, the architecture of
the client objects, and the conventions for this package. This file adds
instructions for coding agents.

## Follow the guide, not the old code

Some old code in rsconnect does not follow the conventions in the
contributing guide. For example, some code uses `<<-`, branches on the
server type, or stores functions as fields on the client objects. Do not
copy a pattern only because nearby code uses it. If the guide and the
existing code disagree, follow the guide. Do not refactor old code that
your task does not touch, unless the user asks you to.

## Code that looks unused

Some functions and client fields have no callers in this repository,
because the RStudio IDE calls them. The IDE also calls some internal
functions with `rsconnect:::`. The contributing guide lists these
functions and fields. Do not remove or rename code because a search
finds no callers. Ask the user first, and remind them that this package
is also used by the RStudio IDE.

## Before you finish

Run these steps after you change code, and fix the problems that they
find:

1.  Run `air format .`.
2.  Run `devtools::test()`. To save time while you work, run only the
    related file with `devtools::test(filter = "...")`, but run the full
    suite at the end.
3.  Run
    [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html).
4.  If you changed roxygen comments, run `devtools::document()`.

Do not try to run the integration tests in `tests/integration` or
`tests/shinyapps-integration`. They need live servers.

If a snapshot test fails, read the diff in `tests/testthat/_snaps/`.
Accept the new snapshot only if the change is intended, and tell the
user which snapshots changed.

## Comments and test names

Write comments about what the code does now. Do not describe old
behavior or how the code changed. Do not put issue or PR numbers in
comments or in `test_that()` descriptions. An exception is a comment
about a workaround for a problem that is not fixed yet, for example an
open bug in a dependency. Such a comment can link to the issue.
