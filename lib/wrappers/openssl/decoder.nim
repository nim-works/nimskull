import libcrypto

import types
export OSSL_DECODER_CTX

proc OSSL_DECODER_CTX_new_for_pkey*(pkey: ptr ptr EVP_PKEY, input_type: cstring, input_struct: cstring, keytype: cstring, selection: cint, libctx: ptr OSSL_LIB_CTX, propquery: cstring): ptr OSSL_DECODER_CTX {.importc, cdecl.}
proc OSSL_DECODER_CTX_free*(ctx: ptr OSSL_DECODER_CTX) {.importc, cdecl.}

proc OSSL_DECODER_from_bio*(ctx: ptr OSSL_DECODER_CTX, `in`: ptr BIO): cint {.importc, cdecl.}
