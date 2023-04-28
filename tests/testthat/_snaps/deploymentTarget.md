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
      i Known servers are "bar".
    Code
      deploymentTarget(account = "john")
    Condition
      Error in `deploymentTarget()`:
      ! Can't find any accounts with `account` = "john".
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
      Please disambiguate by setting `account`.
      i Known account names are "john" and "ron".

# errors/prompts if multiple deployments

    Code
      deploymentTarget(app_dir, appName = "test")
    Condition
      Error:
      ! This directory has been previously deployed in multiple places.
      Please use `appName`, `server` or `account` to disambiguate.
      Known applications:
      * test (server: server1.com / username: ron): <https://server1.com/ron/123>
      * test (server: server2.com / username: ron): <https://server2.com/ron/123>
    Code
      deploymentTarget(app_dir)
    Condition
      Error:
      ! This directory has been previously deployed in multiple places.
      Please use `appName`, `server` or `account` to disambiguate.
      Known applications:
      * test (server: server1.com / username: ron): <https://server1.com/ron/123>
      * test (server: server2.com / username: ron): <https://server2.com/ron/123>

---

    Code
      out <- deploymentTarget(app_dir)
    Message
      This directory has been previously deployed in multiple places.
      Which deployment do you want to use?
      1: test (server: server1.com / username: ron): <https://server1.com/ron/123>
      2: test (server: server2.com / username: ron): <https://server2.com/ron/123>
      Selection: 1

# errors if single deployment and appId doesn't match

    Code
      deploymentTarget(app_dir, appName = "test", appId = "2")
    Condition
      Error:
      ! Supplied `appId` (2) does not match deployment record (1).
      i Omit `appId` to use existing for deployment for app "test", or
      i Omit `appName` to create new deployment record.

# shouldUpdateApp errors when non-interactive

    Code
      shouldUpdateApp(app, "my_app-1")
    Condition
      Error in `shouldUpdateApp()`:
      ! Discovered a previously deployed app named "my_app"
      (View it at <https://example.com>)
      i Set `forceUpdate = TRUE` to update it.
      i Supply a unique `appName` to deploy a new application.

# shouldUpdateApp handles 3 options

    Code
      one <- shouldUpdateApp(app, "my_app-1")
    Message
      Discovered a previously deployed app named "my_app"
      (View it at <https://example.com>)
      What do you want to do?
      1: Update the existing app.
      2: Create a new app with automatically generated name ("my_app-1").
      3: Abort this deployment and supply a custom `appName`.
      Selection: 1
    Code
      two <- shouldUpdateApp(app, "my_app-1")
    Message
      Discovered a previously deployed app named "my_app"
      (View it at <https://example.com>)
      What do you want to do?
      1: Update the existing app.
      2: Create a new app with automatically generated name ("my_app-1").
      3: Abort this deployment and supply a custom `appName`.
      Selection: 2
    Code
      three <- shouldUpdateApp(app, "my_app-1")
    Message
      Discovered a previously deployed app named "my_app"
      (View it at <https://example.com>)
      What do you want to do?
      1: Update the existing app.
      2: Create a new app with automatically generated name ("my_app-1").
      3: Abort this deployment and supply a custom `appName`.
      Selection: 3
    Condition
      Error:
      ! Quiting...

