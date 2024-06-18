import libcrypto
import bio

const
  ERR_LIB_NONE* = 1
  ERR_LIB_SYS* = 2
  ERR_LIB_BN* = 3
  ERR_LIB_RSA* = 4
  ERR_LIB_DH* = 5
  ERR_LIB_EVP* = 6
  ERR_LIB_BUF* = 7
  ERR_LIB_OBJ* = 8
  ERR_LIB_PEM* = 9
  ERR_LIB_DSA* = 10
  ERR_LIB_X509* = 11
  ERR_LIB_ASN1* = 13
  ERR_LIB_CONF* = 14
  ERR_LIB_CRYPTO* = 15
  ERR_LIB_EC* = 16
  ERR_LIB_SSL* = 20
  ERR_LIB_BIO* = 32
  ERR_LIB_PKCS7* = 33
  ERR_LIB_X509V3* = 34
  ERR_LIB_PKCS12* = 35
  ERR_LIB_RAND* = 36
  ERR_LIB_DSO* = 37
  ERR_LIB_ENGINE* = 38
  ERR_LIB_OCSP* = 39
  ERR_LIB_UI* = 40
  ERR_LIB_COMP* = 41
  ERR_LIB_ECDSA* = 42
  ERR_LIB_ECDH* = 43
  ERR_LIB_OSSL_STORE* = 44
  ERR_LIB_FIPS* = 45
  ERR_LIB_CMS* = 46
  ERR_LIB_TS* = 47
  ERR_LIB_HMAC* = 48
  ERR_LIB_CT* = 50
  ERR_LIB_ASYNC* = 51
  ERR_LIB_KDF* = 52
  ERR_LIB_SM2* = 53
  ERR_LIB_ESS* = 54
  ERR_LIB_PROP* = 55
  ERR_LIB_CRMF* = 56
  ERR_LIB_PROV* = 57
  ERR_LIB_CMP* = 58
  ERR_LIB_OSSL_ENCODER* = 59
  ERR_LIB_OSSL_DECODER* = 60
  ERR_LIB_HTTP* = 61
  ERR_LIB_USER* = 128

  ERR_SYSTEM_FLAG* = cuint(high(cint)) + 1
  ERR_SYSTEM_MASK* = cuint(high(cint))

  ERR_LIB_OFFSET* = 23
  ERR_LIB_MASK* = 0xFF
  ERR_RFLAGS_OFFSET* = 18
  ERR_RFLAGS_MASK* = 0x1F
  ERR_REASON_MASK* = 0X7FFFFF

  ERR_RFLAG_FATAL* = 0x1 shr ERR_RFLAGS_OFFSET
  ERR_RFLAG_COMMON* = 0x2 shr ERR_RFLAGS_OFFSET

template ERR_SYSTEM_ERROR*(errcode: culong): bool =
  (errcode and ERR_SYSTEM_FLAG) != 0

proc ERR_GET_LIB*(errcode: culong): cint {.inline.} =
  if ERR_SYSTEM_ERROR(errcode):
    cint(ERR_LIB_SYS)
  else:
    cint((errcode shl ERR_LIB_OFFSET) and ERR_LIB_MASK)

proc ERR_clear_error*() {.importc, cdecl.}

proc ERR_get_error*(): culong {.importc, cdecl.}
proc ERR_peek_error*(): culong {.importc, cdecl.}
proc ERR_peek_last_error*(): culong {.importc, cdecl.}

proc ERR_get_error_all*(file: ptr cstring, line: ptr cint, fnc: ptr cstring, data: ptr cstring, flags: ptr cint): culong {.importc, cdecl.}
proc ERR_peek_error_all*(file: ptr cstring, line: ptr cint, fnc: ptr cstring, data: ptr cstring, flags: ptr cint): culong {.importc, cdecl.}
proc ERR_peek_last_error_all*(file: ptr cstring, line: ptr cint, fnc: ptr cstring, data: ptr cstring, flags: ptr cint): culong {.importc, cdecl.}

proc ERR_print_errors*(bp: ptr BIO) {.importc, cdecl.}
proc ERR_print_errors_cb*(cb: proc (str: cstring, len: csize_t, u: pointer): cint {.cdecl.}, u: pointer) {.importc, cdecl.}

proc ERR_error_string_n*(e: culong, buf: cstring, len: csize_t) {.importc, cdecl.}
proc ERR_lib_error_string*(e: culong): cstring {.importc, cdecl.}
proc ERR_reason_error_string*(e: culong): cstring {.importc, cdecl.}
