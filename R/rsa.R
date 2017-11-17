
# generateToken generates a token for signing requests sent to the RStudio
# Connect service. The token's ID and public key are sent to the server, and
# the private key is saved locally.
generateToken <- function() {
  key <- PKI::PKI.genRSAkey(bits = 2048L)
  priv.der <- PKI::PKI.save.key(key, format = "DER")
  pub.der <- PKI::PKI.save.key(key, format = "DER", private = FALSE)

  # hash the public key to generate the token ID; we used to create a random
  # token ID using sample(), but this causes trouble for users who use a fixed
  # RNG seed and/or need the RNG state to remain unperturbed.
  tokenId <- digest::digest(object = pub.der, algo = "md5")

  # form the token from the
  list(
    token = paste0("T", tokenId),
    public_key = RCurl::base64Encode(pub.der),
    private_key = RCurl::base64Encode(priv.der)
  )
}
