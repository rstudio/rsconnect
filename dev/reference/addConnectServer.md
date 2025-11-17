# Add a server

**\[deprecated\]**

This function does the same thing as
[`addServer()`](https://rstudio.github.io/rsconnect/dev/reference/addServer.md)
so has been removed.

## Usage

``` r
addConnectServer(url, name = NULL, certificate = NULL, quiet = FALSE)
```

## Arguments

- url:

  URL for the server. Can be a bare hostname like
  `connect.mycompany.com` or a url like
  `http://posit.mycompany.com/connect`.

- name:

  Server name. If omitted, the server hostname is used.

- certificate:

  Optional. Either a path to certificate file or a character vector
  containing the certificate's contents.

- quiet:

  Suppress output and prompts where possible.
