# Support for Snowflake key-pair authentication for SPCS and the REST API.
keypair_credentials <- function(
  snowflake_account,
  snowflake_user,
  private_key,
  spcs_endpoint = NULL,
  role = "PUBLIC"
) {
  jwt <- generate_jwt(snowflake_account, snowflake_user, private_key)
  # Important: the SPCS ingress handles key-pair authentication *differently*
  # than the REST API. In particular, we can't pass the signed JWT as a bearer
  # token, we have to go through an exchange step.
  if (is.null(spcs_endpoint)) {
    return(list(
      Authorization = paste("Bearer", jwt),
      # Snowflake requires this additional header for JWTs, presumably to
      # distinguish them from OAuth access tokens.
      `X-Snowflake-Authorization-Token-Type` = "KEYPAIR_JWT"
    ))
  }
  account_url <- sprintf("https://%s.snowflakecomputing.com", snowflake_account)
  token <- exchange_jwt_for_token(account_url, jwt, spcs_endpoint, role)
  # Yes, this is actually the format of the Authorization header that SPCS
  # requires.
  return(list(
    Authorization = sprintf('Snowflake Token="%s"', token)
  ))
}

# Generate a JWT that can be used for Snowflake "key-pair" authentication.
generate_jwt <- function(account, user, private_key, iat = NULL, jti = NULL) {
  key <- openssl::read_key(private_key)
  if (is.null(iat)) {
    iat <- as.integer(Sys.time())
  }
  if (is.null(jti)) {
    jti <- openssl::base64_encode(openssl::rand_bytes(32))
    jti <- gsub("=+$", "", jti)
    jti <- gsub("+", "-", jti, fixed = TRUE)
    jti <- gsub("/", "_", jti, fixed = TRUE)
  }
  # We can't use openssl::fingerprint() here because it uses a different
  # algorithm than Snowflake does.
  fp <- openssl::base64_encode(openssl::sha256(openssl::write_der(key$pubkey)))
  sub <- toupper(paste0(account, ".", user))
  # Note: Snowflake employs a malformed issuer claim, so we have to inject it
  # manually after jose's validation phase.
  claim <- jose::jwt_claim(
    iss = "dummy",
    sub = sub,
    iat = iat,
    # Expire in 5 minutes.
    exp = iat + 300L,
    # TODO: These claims are ignored by Snowflake. Should we omit them?
    nbf = iat,
    jti = jti
  )
  claim$iss <- paste0(sub, ".SHA256:", fp)
  jose::jwt_encode_sig(claim, key)
}

# Exchange a JWT for a Snowflake OAuth access token.
#
# Note: we can't use httr2::oauth_flow_bearer_jwt() because Snowflake does not
# adhere closely enough to RFC 7523. In particular, the response format is not
# JSON, and the JWT uses a malformed issuer claim that jose::jwt_claim() can't
# handle.
#
# See: https://docs.snowflake.com/en/user-guide/oauth-custom#label-oauth-token-exchange
exchange_jwt_for_token <- function(
  account_url,
  jwt,
  spcs_endpoint,
  role = "PUBLIC"
) {
  scope <- sprintf("scope=session:role:%s %s", role, spcs_endpoint)
  grant_type <- "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer"
  assertion <- sprintf("assertion=%s", jwt)
  payload <- paste0(
    utils::URLencode(c(scope, grant_type, assertion)),
    collapse = "&"
  )

  POST(
    service = parseHttpUrl(account_url),
    path = "/oauth/token",
    content = payload,
    authInfo = list(),
    contentType = "application/x-www-form-urlencoded",
    rawResponse = TRUE
  )
}
