# Old Application Config Directory

Returns the old application configuration directory used by rsconnect
0.8.24 and prior. These versions wrote configuration data to XDG
compliant locations, but CRAN policy has since further restricted the
disk locations that are permitted. See:

## Usage

``` r
oldApplicationConfigDir(appName)
```

## Arguments

- appName:

  The application's name (connect or rsconnect)

## Value

The old application configuration directory.

## Details

https://cran.r-project.org/web/packages/policies.html
