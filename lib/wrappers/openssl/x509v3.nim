import libcrypto

import x509

const
  X509_CHECK_FLAG_ALWAYS_CHECK_SUBJECT* = 0x1
  X509_CHECK_FLAG_NO_WILDCARDS* = 0x2
  X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS* = 0x4
  X509_CHECK_FLAG_MULTI_LABEL_WILDCARDS* = 0x8
  X509_CHECK_FLAG_SINGLE_LABEL_SUBDOMAINS* = 0x10
  X509_CHECK_FLAG_NEVER_CHECK_SUBJECT* = 0x20

proc X509_check_host*(x: ptr X509, name: cstring, namelen: csize_t, flags: cuint, peername: ptr cstring): cint {.importc, cdecl.}
