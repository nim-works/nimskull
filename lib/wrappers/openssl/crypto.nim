import libcrypto

import types
export OSSL_LIB_CTX

proc OpenSSL_version_num*(): culong {.importc, cdecl.}
