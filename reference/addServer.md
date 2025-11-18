# Server management

These functions manage the list of known servers:

- `addServer()` registers a Posit connect server. Once it has been
  registered, you can connect to an account on the server using
  [`connectUser()`](https://rstudio.github.io/rsconnect/reference/connectApiUser.md).

- `removeServer()` removes a server from the registry.

- `addServerCertificate()` adds a certificate to a server.

Supported servers: Posit Connect servers

## Usage

``` r
addServer(
  url,
  name = NULL,
  certificate = NULL,
  validate = TRUE,
  snowflakeConnectionName = NULL,
  quiet = FALSE
)

removeServer(name = NULL)

addServerCertificate(name, certificate, quiet = FALSE)
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

- validate:

  Validate that `url` actually points to a Posit Connect server?

- snowflakeConnectionName:

  Name for the Snowflake connection parameters stored in
  `connections.toml`.

- quiet:

  Suppress output and prompts where possible.

## Examples

``` r
if (FALSE) { # \dontrun{
# register a local server
addServer("http://myrsconnect/", "myserver")

# list servers
servers(local = TRUE)

# connect to an account on the server
connectUser(server = "myserver")
} # }
```
