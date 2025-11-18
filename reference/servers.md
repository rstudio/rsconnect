# Server metadata

`servers()` lists all known servers; `serverInfo()` gets metadata about
a specific server. Cloud server `shinyapps.io` is always automatically
registered and available.

Supported servers: All servers

## Usage

``` r
servers(local = FALSE)

serverInfo(name = NULL)
```

## Arguments

- local:

  Return only local servers? (i.e. not automatically registered cloud
  servers)

- name:

  Server name. If omitted, you'll be prompted to pick a server.

## Value

`servers()` returns a data frame with registered server names and URLs.
`serverInfo()` returns a list with details for a particular server.

## Examples

``` r
# List all registered servers
servers()
#>                  name                                url
#> 1        shinyapps.io        https://api.shinyapps.io/v1
#> 2 connect.posit.cloud https://api.connect.posit.cloud/v1
#>            certificate
#> 1 Amazon... (redacted)
#> 2 -----B... (redacted)

# Get information about a server
serverInfo("shinyapps.io")
#> $name
#> [1] "shinyapps.io"
#> 
#> $url
#> [1] "https://api.shinyapps.io/v1"
#> 
#> $certificate
#> [1] "Amazon... (redacted)"
#> 
```
