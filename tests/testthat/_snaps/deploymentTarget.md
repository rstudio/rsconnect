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

