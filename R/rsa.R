
# generateToken generates a token for signing requests sent to the RStudio
# Connect service. The token's ID and public key are sent to the server, and
# the private key is saved locally.
generateToken <- function() {
  key <- PKI::PKI.genRSAkey(bits = 2048L)
  priv.der <- PKI::PKI.save.key(key, format = "DER")
  pub.der <- PKI::PKI.save.key(key, format = "DER", private = FALSE)
  list(
    token = digest::digest(pub.der, algo = "md5"),
    public_key = RCurl::base64Encode(pub.der),
    private_key = RCurl::base64Encode(priv.der)
  )
}
