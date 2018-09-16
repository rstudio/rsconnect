
# generateToken generates a token for signing requests sent to the RStudio
# Connect service. The token's ID and public key are sent to the server, and
# the private key is saved locally.
generateToken <- function() {
  key <- openssl::rsa_keygen(2048L)
  priv.der <- openssl::write_der(key)
  pub.der <- openssl::write_der(key$pubkey)

  # hash the public key to generate the token ID; we used to create a random
  # token ID using sample(), but this causes trouble for users who use a fixed
  # RNG seed and/or need the RNG state to remain unperturbed.
  tokenId <- as.character(openssl::md5(serialize(pub.der, NULL)))

  # form the token from the
  list(
    token = paste0("T", tokenId),
    public_key = openssl::base64_encode(pub.der),
    private_key = openssl::base64_encode(priv.der)
  )
}
