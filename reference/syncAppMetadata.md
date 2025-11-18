# Update deployment records

Update the deployment records for applications published to Posit
Connect. This updates application title and URL, and deletes records for
deployments where the application has been deleted on the server.

Supported servers: Posit Connect servers

## Usage

``` r
syncAppMetadata(appPath = ".")
```

## Arguments

- appPath:

  The path to the directory or file that was deployed.
