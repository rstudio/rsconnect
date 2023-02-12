# errors if no accounts

    Code
      deploymentTarget()
    Error <simpleError>
      You must register an account using setAccountInfo prior to proceeding.

# errors if unknown server

    Code
      deploymentTarget(server = "baz")
    Error <simpleError>
      You must register an account using setAccountInfo prior to proceeding.

# errors if bad account

    Code
      deploymentTarget(server = NULL, account = "john")
    Error <simpleError>
      Unknown account name 'john' (you can use the setAccountInfo function to add a new account)

# errors if no previous deployments and multiple accounts

    Code
      deploymentTarget(app_dir)
    Error <simpleError>
      Please specify the account and server to which you want to deploy the application (there is more than one account registered on this system).
    Code
      deploymentTarget(app_dir, appName = "test")
    Error <simpleError>
      Please specify the account name (there is more than one account registered on this system)

# handles accounts if only server specified

    Code
      deploymentTarget(app_dir, server = "foo")
    Error <simpleError>
      Please specify the account name (there is more than one account registered on this system)

# errors if multiple deployments

    Code
      deploymentTarget(app_dir, appName = "test")
    Error <simpleError>
      Please specify the account you want to deploy 'test' to (you have previously deployed this application to more than one account).
    Code
      deploymentTarget(app_dir)
    Error <simpleError>
      Unable to deploy using default arguments (multiple existing deployments from this application directory already exist). Please specify appName and/or account name explicitly.

