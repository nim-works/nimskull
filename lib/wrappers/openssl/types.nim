type
  BIO* {.final.} = object
  ENGINE* {.pure.} = object
  EVP_MD* {.pure.} = object
  EVP_PKEY* {.pure.} = object
  EVP_PKEY_CTX* {.pure.} = object
  OSSL_DECODER_CTX* {.pure.} = object
  OSSL_LIB_CTX* {.pure.} = object
  SSL* {.pure.} = object
  SSL_CTX* {.pure.} = object
  pem_password_cb* = proc (buf: cstring, size: cint, rwflag: cint, u: pointer): cint {.cdecl.}

when defined(nimOpenssl111):
  type
    RSA* {.pure.} = object
