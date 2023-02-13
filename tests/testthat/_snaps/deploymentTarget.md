# errors if no accounts

    Code
      deploymentTarget()
    Condition
      Error in `deploymentTarget()`:
      ! No accounts registered.
      i Call `rsconnect::setAccountInfo()` to register an account.

# errors if unknown account or server

    Code
      deploymentTarget(server = "unknown")
    Condition
      Error in `deploymentTarget()`:
      ! Can't find any accounts with `server` = "unknown".
      i Available servers: "bar".
    Code
      deploymentTarget(account = "john")
    Condition
      Error in `deploymentTarget()`:
      ! Can't find any accounts with `acccount` = "john".
      i Available account names: "foo".

# errors if no previous deployments and multiple accounts

    Code
      deploymentTarget(app_dir)
    Condition
      Error in `deploymentTarget()`:
      ! Found multiple accounts.
      Please disambiguate by setting `server` and/or `account`.
      i Available servers: "foo1" and "foo2".
      i Available account names: "ron".
    Code
      deploymentTarget(app_dir, appName = "test")
    Condition
      Error in `deploymentTarget()`:
      ! Found multiple accounts.
      Please disambiguate by setting `server` and/or `account`.
      i Available servers: "foo1" and "foo2".
      i Available account names: "ron".

# handles accounts if only server specified

    Code
      deploymentTarget(app_dir, server = "foo")
    Condition
      Error in `deploymentTarget()`:
      ! Found multiple accounts for `server` = "foo".
      Please disambiguate by setting `acccount`.
      i Available account names: "ron" and "john".

# errors if multiple deployments

    Code
      deploymentTarget(app_dir, appName = "test")
    Condition
      Error:
      ! This app has been previously deployed in multiple places.
      Please use `appName`, `server` or `account` to disambiguate.
      i Known application names: "test".
      i Known servers: "foo1" and "foo2".
      i Known account names: "ron".
    Code
      deploymentTarget(app_dir)
    Condition
      Error:
      ! This app has been previously deployed in multiple places.
      Please use `appName`, `server` or `account` to disambiguate.
      i Known application names: "test".
      i Known servers: "foo1" and "foo2".
      i Known account names: "ron".

