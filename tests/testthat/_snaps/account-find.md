# validates its arguments

    Code
      findAccount(1, NULL)
    Condition
      Error:
      ! `account` must be a single string or `NULL`, not the number 1.
    Code
      findAccount(NULL, 1)
    Condition
      Error:
      ! `server` must be a single string or `NULL`, not the number 1.

# error if no accounts

    Code
      findAccount()
    Condition
      Error:
      ! No accounts registered.
      i Call `rsconnect::setAccountInfo()` to register an account.

# error if no matching account

    Code
      findAccount("unknown", NULL)
    Condition
      Error:
      ! Can't find any accounts with `account` = "unknown".
      i Available account names: "name".
    Code
      findAccount(NULL, "unknown")
    Condition
      Error:
      ! Can't find any accounts with `server` = "unknown".
      i Available servers: "server".
    Code
      findAccount("unknown", "unknown")
    Condition
      Error:
      ! Can't find account with `name` = "unknown" and `server` = "unknown"
      i Call `accounts()` to see available options.

# error if ambiguous accounts in non-interactive environment

    Code
      findAccount()
    Condition
      Error:
      ! Found multiple accounts.
      Please disambiguate by setting `server` and/or `account`.
      i Available servers: "x" and "y".
      i Available account names: "a" and "b".
    Code
      findAccount("a", NULL)
    Condition
      Error:
      ! Found multiple accounts for `account` = "a".
      Please disambiguate by setting `server`.
      i Available servers: "x" and "y".
    Code
      findAccount(NULL, "y")
    Condition
      Error:
      ! Found multiple accounts for `server` = "y".
      Please disambiguate by setting `account`.
      i Available account names: "a" and "b".

# prompted to pick account in interactive environment

    Code
      out <- findAccount()
    Message
      Found multiple accounts.
      Which one do you want to use?
      1: a@x
      2: a@y
      3: b@y
      Selection: 2

