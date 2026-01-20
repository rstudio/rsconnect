test_that("isSPCSServer correctly identifies Snowpark Container Services servers", {
  local_temp_config()

  # Register a server with a snowflakecomputing.app URL
  addTestServer(
    url = "https://test-abc123.snowflakecomputing.app/__api__",
    name = "spcs_server"
  )

  # Mock serverInfo to return the URL for our test
  local_mocked_bindings(
    serverInfo = function(server) {
      if (server == "spcs_server") {
        return(list(url = "https://test-abc123.snowflakecomputing.app/__api__"))
      }
      list(url = "https://example.com")
    }
  )

  expect_true(isSPCSServer("spcs_server"))
  expect_false(isSPCSServer("example.com"))
})

test_that("authHeaders handles snowflakeToken", {
  authInfo <- list(
    snowflakeToken = list(
      Authorization = "Snowflake Token=\"mock_token\"",
      `X-Custom-Header` = "custom-value"
    )
  )

  headers <- authHeaders(authInfo, "GET", "/path")

  expect_equal(headers$Authorization, "Snowflake Token=\"mock_token\"")
  expect_equal(headers$`X-Custom-Header`, "custom-value")
})

test_that("authHeaders handles snowflakeToken with API key", {
  authInfo <- list(
    apiKey = secret("the-api-key"),
    snowflakeToken = list(
      Authorization = "Snowflake Token=\"mock_token\"",
      `X-Custom-Header` = "custom-value"
    )
  )

  # Test authHeaders
  headers <- authHeaders(authInfo, "GET", "/path")

  # Verify snowflakeToken headers were used
  expect_equal(headers$Authorization, "Snowflake Token=\"mock_token\"")
  expect_equal(headers$`X-RSC-Authorization`, "Key the-api-key")
  expect_equal(headers$`X-Custom-Header`, "custom-value")
})

test_that("authHeaders handles snowflakeToken with token + secret", {
  local_mocked_bindings(
    rfc2616Date = function() "Thu, 09 Mar 2023 14:29:00 GMT"
  )

  authInfo <- list(
    token = "connect-token-123",
    secret = openssl::base64_encode("shared-secret"),
    snowflakeToken = list(
      Authorization = "Snowflake Token=\"mock_token\""
    )
  )

  headers <- authHeaders(authInfo, "GET", "/path")

  # Verify snowflakeToken header
  expect_equal(headers$Authorization, "Snowflake Token=\"mock_token\"")

  # Verify signature headers are present
  expect_equal(headers$`X-Auth-Token`, "connect-token-123")
  expect_equal(headers$Date, "Thu, 09 Mar 2023 14:29:00 GMT")
  expect_true(!is.null(headers$`X-Auth-Signature`))
  expect_true(!is.null(headers$`X-Content-Checksum`))
})

test_that("authHeaders handles snowflakeToken with token + private_key", {
  local_mocked_bindings(
    rfc2616Date = function() "Thu, 09 Mar 2023 14:29:00 GMT"
  )

  # Use same test key as test-http.R
  key_string <- "-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQC/eiSQAKXADslq
GGsbsQu2eEgEHD06BtUhaeU1nvsY7a6u12xpG0OAGYWhnGhR+1K/3qoZQQNmN0MC
ZV0zhueREu3YaqtNwXbnbGCqp7tsLsx2cb2TZscmBNXkOLah2PlsBTfInNlKrpKH
wsMtsL7yruXwJ767ey8JujMAqO90jj57idfhkr4IU47EL8DiIHTOAYLddKe/d4AL
skoEFQM37SJ9JUwZUsvz2eqEXFPV4QrS93T79sA7QfH5zOYTLKPEDxV0U7AYUVKW
bBieVRlu3DIQfdsqXwYYzHUR+HBlxe1VowCS5p5c6coNLdb+RElcSSY2Gd06eBg0
wSoxkyT3AgMBAAECggEAHmQinYCczkkKiv5pMbH+K+4XcB+TUDx5Y55NNR+Qtzoy
eanTmTMsmY5zeV076Zc8YRvUX8HD6ltnFWBFVMJaytn4SltT6TmFx+GZzjxlMRyU
c1BGSLkNbulhkaG2yyWHITAK1Jqgmovu0gGFvSDKjfZYpK+KRHOe2apmIfquVw9d
6CLm7swx30x8qFKEACc3iM/Mcc9uKOWn+NjVKqUVfU/9ZmNxmjZ9e3wkwieI1mSB
gq9q7fmCHHkGWbUkGMphgRyssaJbz+bn/Wz2uwevvtebkTOzvWtKhUGgDByCCCtq
J1ehPCjdRjylb6C5tLbyeng5QnYC7uZJmRsEOUwMEQKBgQD6PsHXwKJ8JRlDGu4G
JQ9fKzYA+B+No0GKtGuuRDD8tJouj5rb1dZt181UHUW/UtjNpz8j8l1RN2gPcu/v
VjwSXDcZHohv4cgQfRCN47wREEEb/LP/fhxIt2H320vh5qvwdJJoGvGoJfR5vO1X
ysdIPBajIgnEo4U7cawaNfeS1QKBgQDD4WgXUb0AYvK6lggVVqgNzxxMQRWCQSGb
y4RISHlC2TpftpbLFdr6fHuf8bzGq462xG5MFMMGBsbESXKNW18QA+uIBIDwttaj
AfQ4+PNu4m+2Ump+RcGu2MYoBJoxjMx00Ba76cEMF0+X+RO4zcfwZ6Y869Fakq+D
7rn4ZetGmwKBgQCMtgsjeUMkUWwCCrt6ow4gslh8dQixCPKKvuapp9hv0FG+CqvG
H1iijSz8tjUI3tnf0cI0QUztpR0TSsrVpoTCwi2NJ1kKqEdp1hkf38VZRu2FgjPo
Xw4iaVNiHmJt1NorrDDC7xuhNC5i4bQHoJMr7/W+px4c/uGkykc+ucfLPQKBgQCG
/E/KOibgHFAfawLZCaW4FnDuz68t2wp5HY/kbCU8fwxuJxrVixMjqSNcfq9TzagE
pWtI/MnE3midnevWJBBnrfvi+Q+OUsGpBdCybkT7tgm8ACGpMRMfFf3AWCOWX+wJ
19jC2HyTg4DzPs9rfEv7jMIPm4bjPtC7P4li94FiXwKBgBTh5tPUEvG0chZvqRT2
g0vvWgJGF52FCXBij3dnNl1eNRQYbDI+hNbZYcHCKHKaOoDWaqYhyjLk6Tz0LhLe
XWAlOP6tE2UbEgi10wyaEI9EyfXg1mgiHlSg+oZMCx05TUE6PrzddS6qUOJfN7P3
a3hEFijsjg/+FDMr+iAVzjry
-----END PRIVATE KEY-----"
  key <- openssl::base64_encode(openssl::read_key(key_string))

  authInfo <- list(
    token = "connect-token-456",
    private_key = key,
    snowflakeToken = list(
      Authorization = "Snowflake Token=\"mock_token\""
    )
  )

  headers <- authHeaders(authInfo, "GET", "/path")

  # Verify snowflakeToken header
  expect_equal(headers$Authorization, "Snowflake Token=\"mock_token\"")

  # Verify signature headers are present
  expect_equal(headers$`X-Auth-Token`, "connect-token-456")
  expect_equal(headers$Date, "Thu, 09 Mar 2023 14:29:00 GMT")
  expect_true(!is.null(headers$`X-Auth-Signature`))
  expect_true(!is.null(headers$`X-Content-Checksum`))
})

test_that("registerAccount stores snowflakeConnectionName", {
  local_temp_config()

  # Register an account with snowflakeConnectionName
  registerAccount(
    serverName = "example.com",
    accountName = "testuser",
    accountId = "user123",
    snowflakeConnectionName = "test_connection"
  )

  # Check the account info has the snowflakeConnectionName
  info <- accountInfo("testuser", "example.com")
  expect_equal(info$snowflakeConnectionName, "test_connection")
})

test_that("registerAccount stores both apiKey and snowflakeConnectionName for SPCS accounts", {
  local_temp_config()

  # Register an SPCS account with both apiKey and snowflakeConnectionName
  registerAccount(
    serverName = "spcs.example.com",
    accountName = "spcsuser",
    accountId = "user456",
    apiKey = "test-api-key-789",
    snowflakeConnectionName = "spcs_connection"
  )

  # Check the account info has both fields
  info <- accountInfo("spcsuser", "spcs.example.com")
  expect_equal(info$snowflakeConnectionName, "spcs_connection")
  expect_equal(as.character(info$apiKey), "test-api-key-789")
})
