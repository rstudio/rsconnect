# createUniqueId creates a random hex string of length bytes.
# Uses its own random state.
#
# createUniqueId is used to create the token (name) for a public/private
# key-pair. We have used three approaches for token generation:
#
# 1. Random hex string using the incoming random state. Produced duplicate
#    tokens when users forced a constant initial seed (set.seed(0)).
#    https://github.com/rstudio/rsconnect/issues/221
# 2. MD5 of the public key. Produced empty tokens when the MD5 algorithm
#    is disabled (due to FIPS mode).
#    https://github.com/rstudio/rsconnect/issues/378
# 3. Random hex string with isolated random state.
createUniqueId <- function(bytes) {
  # Seed handling inspired by:
  # http://www.cookbook-r.com/Numbers/Saving_the_state_of_the_random_number_generator/
  if (exists(".Random.seed", .GlobalEnv)) {
    oldseed <- .GlobalEnv$.Random.seed
  } else {
    oldseed <- NULL
  }
  on.exit(
    if (!is.null(oldseed)) {
      .GlobalEnv$.Random.seed <- oldseed
    } else {
      rm(".Random.seed", envir = .GlobalEnv)
    },
    add = TRUE
  )

  set.seed(NULL)
  paste0(as.hexmode(sample(256, bytes) - 1), collapse = "")
}

# generateToken generates a token for signing requests sent to the Posit
# Connect service. The token's ID and public key are sent to the server, and
# the private key is saved locally.
generateToken <- function() {
  key <- openssl::rsa_keygen(2048L)
  priv.der <- openssl::write_der(key)
  pub.der <- openssl::write_der(key$pubkey)
  tokenId <- createUniqueId(16)

  list(
    token = paste0("T", tokenId),
    public_key = openssl::base64_encode(pub.der),
    private_key = openssl::base64_encode(priv.der)
  )
}
