# showUsers errors on a non-shinyapps, non-PCC server

    Code
      showUsers(appName = "myapp", account = "myaccount", server = "connect.example.com")
    Condition
      Error:
      ! rsconnect can't manage application users on Posit Connect.

# addAuthorizedUser() aborts targeting a Posit Connect server

    Code
      addAuthorizedUser("alice@example.com", appDir = appDir, appName = "myapp",
        account = "connect-user", server = "connect-server")
    Condition
      Error:
      ! rsconnect can't manage application users on Posit Connect.

# removeAuthorizedUser() aborts targeting a Posit Connect server

    Code
      removeAuthorizedUser("alice@example.com", appDir = appDir, appName = "myapp",
        account = "connect-user", server = "connect-server")
    Condition
      Error:
      ! rsconnect can't manage application users on Posit Connect.

# showInvited() aborts targeting a Posit Connect server

    Code
      showInvited(appDir = appDir, appName = "myapp", account = "connect-user",
        server = "connect-server")
    Condition
      Error:
      ! rsconnect can't manage application users on Posit Connect.

# resendInvitation() aborts targeting a Posit Connect server

    Code
      resendInvitation("alice@example.com", appDir = appDir, appName = "myapp",
        account = "connect-user", server = "connect-server")
    Condition
      Error:
      ! rsconnect can't manage application users on Posit Connect.

# showUsers() aborts targeting a Posit Connect server

    Code
      showUsers(appDir = appDir, appName = "myapp", account = "connect-user", server = "connect-server")
    Condition
      Error:
      ! rsconnect can't manage application users on Posit Connect.

