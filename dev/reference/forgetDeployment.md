# Forget Application Deployment

Forgets about an application deployment. This is useful if the
application has been deleted on the server, or the local deployment
information needs to be reset.

Supported servers: All servers

## Usage

``` r
forgetDeployment(
  appPath = getwd(),
  name = NULL,
  account = NULL,
  server = NULL,
  dryRun = FALSE,
  force = !interactive()
)
```

## Arguments

- appPath:

  The path to the content that was deployed, either a directory or an
  individual document.

- name:

  The name of the content that was deployed (optional)

- account:

  The name of the account to which the content was deployed (optional)

- server:

  The name of the server to which the content was deployed (optional)

- dryRun:

  Set to TRUE to preview the files/directories to be removed instead of
  actually removing them. Defaults to FALSE.

- force:

  Set to TRUE to remove files and directories without prompting.
  Defaults to FALSE in interactive sessions.

## Value

NULL, invisibly.

## Details

This method removes from disk the file containing deployment metadata.
If "name", "account", and "server" are all NULL, then all of the
deployments for the application are forgotten; otherwise, only the
specified deployment is forgotten.
