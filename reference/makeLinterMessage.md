# Construct a Linter Message

Pretty-prints a linter message. Primarily used as a helper for
constructing linter messages with
[`linter()`](https://rstudio.github.io/rsconnect/reference/linter.md).

Supported servers: All servers

## Usage

``` r
makeLinterMessage(header, content, lines)
```

## Arguments

- header:

  A header message describing the linter.

- content:

  The content of the file that was linted.

- lines:

  The line numbers from `content` that contain lint.
