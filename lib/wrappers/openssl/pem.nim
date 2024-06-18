import types

export pem_password_cb

when defined(nimOpenssl111):
  proc PEM_read_bio_RSA_PUBKEY*(bp: ptr BIO, x: ptr ptr RSA, cb: pem_password_cb, u: pointer): ptr RSA {.importc, cdecl.}
  proc PEM_read_bio_RSAPrivateKey*(bp: ptr BIO, x: ptr ptr RSA, cb: pem_password_cb, u: pointer): ptr RSA {.importc, cdecl.}
