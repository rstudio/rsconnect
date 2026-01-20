# Register account on Posit Connect in Snowpark Container Services

`connectSPCSUser()` connects your Posit Connect account to the rsconnect
package so it can deploy and manage applications on your behalf.
Configure a [`connections.toml`
file](https://docs.snowflake.com/en/developer-guide/snowflake-cli/connecting/configure-cli#location-of-the-toml-configuration-fil)
in the appropriate location.

SPCS deployments require both Snowflake authentication (via the
connection name) and a Posit Connect API key. The Snowflake token
provides proxied authentication to reach the Connect server, while the
API key identifies the user to Connect itself.

If `snowflakeConnectionName` is not provided, rsconnect will attempt to
use the default Snowflake connection from the `connections.toml` file,
provided that the account matches the Connect server's URL.

Supported servers: Posit Connect servers

## Usage

``` r
connectSPCSUser(
  account = NULL,
  server = NULL,
  apiKey,
  snowflakeConnectionName = NULL,
  quiet = FALSE
)
```

## Arguments

- account:

  A name for the account to connect.

- server:

  The server to connect to.

- apiKey:

  The API key used to authenticate the user

- snowflakeConnectionName:

  Name of the Snowflake connection in `connections.toml` to use for
  authentication or `NULL` to use the default (when applicable).

- quiet:

  Whether or not to show messages and prompts while connecting the
  account.
