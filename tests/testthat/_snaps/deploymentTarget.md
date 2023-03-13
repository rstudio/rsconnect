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
      * test (ron@server1.com): <http://server1.com/test>
      * test (ron@server2.com): <http://server2.com/test>
    Code
      deploymentTarget(app_dir)
    Condition
      Error:
      ! This directory has been previously deployed in multiple places.
      Please use `appName`, `server` or `account` to disambiguate.
      Known applications:
      * test (ron@server1.com): <http://server1.com/test>
      * test (ron@server2.com): <http://server2.com/test>

---

    Code
      out <- deploymentTarget(app_dir)
    Message
      This directory has been previously deployed in multiple places.
      Which deployment do you want to use?
      1: test (ron@server1.com): <http://server1.com/test>
      2: test (ron@server2.com): <http://server2.com/test>
      Selection: 1

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

