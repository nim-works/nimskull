import crypto
import engine
import obj_mac
import types

export EVP_MD, EVP_PKEY, EVP_PKEY_CTX

const
  EVP_PKEY_NONE* = NID_undef
  EVP_PKEY_RSA* = NID_rsaEncryption
  EVP_PKEY_RSA2* = NID_rsa
  EVP_PKEY_RSA_PSS* = NID_rsassaPss
  EVP_PKEY_DSA* = NID_dsa
  EVP_PKEY_DSA1* = NID_dsa_2
  EVP_PKEY_DSA2* = NID_dsaWithSHA
  EVP_PKEY_DSA3* = NID_dsaWithSHA1
  EVP_PKEY_DSA4* = NID_dsaWithSHA1_2
  EVP_PKEY_DH* = NID_dhKeyAgreement
  EVP_PKEY_DHX* = NID_dhpublicnumber
  EVP_PKEY_EC* = NID_X9_62_id_ecPublicKey
  EVP_PKEY_SM2* = NID_sm2
  EVP_PKEY_HMAC* = NID_hmac
  EVP_PKEY_CMAC* = NID_cmac
  EVP_PKEY_SCRYPT* = NID_id_scrypt
  EVP_PKEY_TLS1_PRF* = NID_tls1_prf
  EVP_PKEY_HKDF* = NID_hkdf
  EVP_PKEY_POLY1305* = NID_poly1305
  EVP_PKEY_SIPHASH* = NID_siphash
  EVP_PKEY_X25519* = NID_X25519
  EVP_PKEY_ED25519* = NID_ED25519
  EVP_PKEY_X448* = NID_X448
  EVP_PKEY_ED448* = NID_ED448
  EVP_PKEY_KEYMGMT* = -1

proc EVP_PKEY_CTX_new*(pkey: ptr EVP_PKEY, engine: ptr ENGINE): ptr EVP_PKEY_CTX {.importc, cdecl.}
proc EVP_PKEY_CTX_new_from_pkey*(libctx: ptr OSSL_LIB_CTX, pkey: ptr EVP_PKEY, propquery: cstring): ptr EVP_PKEY_CTX {.importc, cdecl.}
proc EVP_PKEY_CTX_free*(ctx: ptr EVP_PKEY_CTX) {.importc, cdecl.}

proc EVP_PKEY_new*(): ptr EVP_PKEY {.importc, cdecl.}
proc EVP_PKEY_free*(key: ptr EVP_PKEY) {.importc, cdecl.}

when defined(nimOpenssl111):
  proc EVP_PKEY_assign*(pkey: ptr EVP_PKEY, typ: cint, key: pointer): cint {.importc, cdecl.}
  proc EVP_PKEY_assign_RSA*(pkey: ptr EVP_PKEY, rsa: ptr RSA): cint {.inline.} =
    EVP_PKEY_assign(pkey, EVP_PKEY_RSA, rsa)

proc EVP_PKEY_encrypt_init*(ctx: ptr EVP_PKEY_CTX): cint {.importc, cdecl.}
proc EVP_PKEY_encrypt*(ctx: ptr EVP_PKEY_CTX, `out`: ptr UncheckedArray[cuchar], outlen: ptr csize_t, `in`: ptr UncheckedArray[cuchar], inlen: csize_t): cint {.importc, cdecl.}

proc EVP_PKEY_decrypt_init*(ctx: ptr EVP_PKEY_CTX): cint {.importc, cdecl.}
proc EVP_PKEY_decrypt*(ctx: ptr EVP_PKEY_CTX, `out`: ptr UncheckedArray[cuchar], outlen: ptr csize_t, `in`: ptr UncheckedArray[cuchar], inlen: csize_t): cint {.importc, cdecl.}

proc EVP_PKEY_CTX_ctrl*(ctx: ptr EVP_PKEY_CTX, keytype: cint, optype: cint, cmd: cint, p1: cint, p2: pointer): cint {.importc, cdecl.}
proc EVP_PKEY_CTX_ctrl_uint64*(ctx: ptr EVP_PKEY_CTX, keytype: cint, optype: cint, cmd: cint, value: uint64): cint {.importc, cdecl.}
proc EVP_PKEY_CTX_ctrl_str*(ctx: ptr EVP_PKEY_CTX, typ: cstring, value: cstring): cint {.importc, cdecl.}
