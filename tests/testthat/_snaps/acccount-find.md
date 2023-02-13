# validates its arguments

    Code
      findAccount(1, NULL)
    Error <rlang_error>
      `account` must be a single string or `NULL`, not the number 1.
    Code
      findAccount(NULL, 1)
    Error <rlang_error>
      `server` must be a single string or `NULL`, not the number 1.

# error if no accounts

    Code
      findAccount()
    Error <rlang_error>
      No accounts registered.
      i Call `rsconnect::setAccountInfo()` to register an account.

# error if no matching account

    Code
      findAccount("unknown", NULL)
    Error <rlang_error>
      Can't find any accounts with `acccount` = "unknown".
      i Available account names: "name".
    Code
      findAccount(NULL, "unknown")
    Error <rlang_error>
      Can't find any accounts with `server` = "unknown".
      i Available servers: "server".
    Code
      findAccount("unknown", "unknown")
    Error <rlang_error>
      Can't find account with `name` = "unknown" and `server` = "unknown"
      i Call `accounts()` to see available options.

# error if ambiguous accounts

    Code
      findAccount()
    Error <rlang_error>
      Found multiple accounts.
      Please disambiguate by setting `server` and/or `account`.
      i Available servers: "x" and "y".
      i Available account names: "a" and "b".
    Code
      findAccount("a", NULL)
    Error <rlang_error>
      Found multiple accounts for `account` = "a".
      Please disambiguate by setting `server`.
      i Available servers: "x" and "y".
    Code
      findAccount(NULL, "y")
    Error <rlang_error>
      Found multiple accounts for `server` = "y".
      Please disambiguate by setting `acccount`.
      i Available account names: "a" and "b".

