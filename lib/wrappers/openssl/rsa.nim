import types

const
  RSA_PKCS1_PADDING* = 1
  RSA_NO_PADDING* = 3
  RSA_PKCS1_OAEP_PADDING* = 4
  RSA_X931_PADDING* = 5
  RSA_PKCS1_PSS_PADDING* = 6
  RSA_PKCS1_WITH_TLS_PADDING* = 7

proc EVP_PKEY_CTX_set_rsa_padding*(ctx: ptr EVP_PKEY_CTX, pad: cint): cint {.importc, cdecl.}
proc EVP_PKEY_CTX_get_rsa_padding*(ctx: ptr EVP_PKEY_CTX, pad: ptr cint): cint {.importc, cdecl.}

when defined(nimOpenssl111):
  proc RSA_new*(): ptr RSA {.importc, cdecl.}
  proc RSA_free*(rsa: ptr RSA) {.importc, cdecl.}
