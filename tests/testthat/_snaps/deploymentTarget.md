# errors if no accounts

    Code
      deploymentTarget()
    Error <rlang_error>
      No accounts registered.
      i Call `rsconnect::setAccountInfo()` to register an account.

# errors if unknown account or server

    Code
      deploymentTarget(server = "unknown")
    Error <rlang_error>
      Can't find any accounts with `server` = "unknown".
      i Available servers: "bar".
    Code
      deploymentTarget(account = "john")
    Error <rlang_error>
      Can't find any accounts with `acccount` = "john".
      i Available account names: "foo".

# errors if no previous deployments and multiple accounts

    Code
      deploymentTarget(app_dir)
    Error <rlang_error>
      Found multiple accounts.
      Please disambiguate by setting `server` and/or `account`.
      i Available servers: "foo1" and "foo2".
      i Available account names: "ron".
    Code
      deploymentTarget(app_dir, appName = "test")
    Error <rlang_error>
      Found multiple accounts.
      Please disambiguate by setting `server` and/or `account`.
      i Available servers: "foo1" and "foo2".
      i Available account names: "ron".

# handles accounts if only server specified

    Code
      deploymentTarget(app_dir, server = "foo")
    Error <rlang_error>
      Found multiple accounts for `server` = "foo".
      Please disambiguate by setting `acccount`.
      i Available account names: "ron" and "john".

# errors if multiple deployments

    Code
      deploymentTarget(app_dir, appName = "test")
    Error <rlang_error>
      This app has been previously deployed in multiple places.
      Please use `server` or `account` to disambiguate.
      i Known servers: "foo1" and "foo2".
      i Known account names: "ron".
    Code
      deploymentTarget(app_dir)
    Error <rlang_error>
      This app has been previously deployed in multiple places.
      Please use `server` or `account` to disambiguate.
      i Known servers: "foo1" and "foo2".
      i Known account names: "ron".

