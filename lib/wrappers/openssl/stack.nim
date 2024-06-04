import libcrypto

type
  OPENSSL_STACK* {.pure.} = object
  OPENSSL_sk_compfunc* = proc (a: pointer, b: pointer): cint {.cdecl.}
  OPENSSL_sk_copyfunc* = proc (p: pointer): pointer {.cdecl.}
  OPENSSL_sk_freefunc* = proc (p: pointer) {.cdecl.}

proc OPENSSL_sk_new*(cmp: OPENSSL_sk_compfunc): ptr OPENSSL_STACK {.importc, cdecl.}
proc OPENSSL_sk_new_null*(): ptr OPENSSL_STACK {.importc, cdecl.}
proc OPENSSL_sk_new_reserve*(c: OPENSSL_sk_compfunc, n: cint): ptr OPENSSL_STACK {.importc, cdecl.}
proc OPENSSL_sk_free*(st: ptr OPENSSL_STACK) {.importc, cdecl.}
proc OPENSSL_sk_pop_free*(st: ptr OPENSSL_STACK, fn: OPENSSL_sk_freefunc) {.importc, cdecl.}
proc OPENSSL_sk_deep_copy*(st: ptr OPENSSL_STACK, c: OPENSSL_sk_copyfunc, f: OPENSSL_sk_freefunc): ptr OPENSSL_STACK {.importc, cdecl.}
proc OPENSSL_sk_dup*(st: ptr OPENSSL_STACK): ptr OPENSSL_STACK {.importc, cdecl.}

proc OPENSSL_sk_delete*(st: ptr OPENSSL_STACK, loc: cint): pointer {.importc, cdecl.}
proc OPENSSL_sk_delete_ptr*(st: ptr OPENSSL_STACK, p: pointer): pointer {.importc, cdecl.}
proc OPENSSL_sk_find*(st: ptr OPENSSL_STACK, data: pointer): cint {.importc, cdecl.}
proc OPENSSL_sk_find_all*(st: ptr OPENSSL_STACK, data: pointer, pnum: ptr cint): cint {.importc, cdecl.}
proc OPENSSL_sk_find_ex*(st: ptr OPENSSL_STACK, data: pointer): cint {.importc, cdecl.}
proc OPENSSL_sk_insert*(st: ptr OPENSSL_STACK, data: pointer, where: cint): cint {.importc, cdecl.}
proc OPENSSL_sk_is_sorted*(st: ptr OPENSSL_STACK): cint {.importc, cdecl.}
proc OPENSSL_sk_num*(st: ptr OPENSSL_STACK): cint {.importc, cdecl.}
proc OPENSSL_sk_pop*(st: ptr OPENSSL_STACK): pointer {.importc, cdecl.}
proc OPENSSL_sk_push*(st: ptr OPENSSL_STACK, data: pointer): cint {.importc, cdecl.}
proc OPENSSL_sk_reserve*(st: ptr OPENSSL_STACK, n: cint): cint {.importc, cdecl.}
proc OPENSSL_sk_set*(st: ptr OPENSSL_STACK, i: cint, data: pointer): pointer {.importc, cdecl.}
proc OPENSSL_sk_set_cmp_func*(st: ptr OPENSSL_STACK, cmp: OPENSSL_sk_compfunc): OPENSSL_sk_compfunc {.importc, cdecl.}
proc OPENSSL_sk_shift*(st: ptr OPENSSL_STACK): pointer {.importc, cdecl.}
proc OPENSSL_sk_sort*(st: ptr OPENSSL_STACK) {.importc, cdecl.}
proc OPENSSL_sk_unshift*(st: ptr OPENSSL_STACK, data: pointer): cint {.importc, cdecl.}
proc OPENSSL_sk_value*(st: ptr OPENSSL_STACK, i: cint): pointer {.importc, cdecl.}
proc OPENSSL_sk_zero*(st: ptr OPENSSL_STACK) {.importc, cdecl.}
