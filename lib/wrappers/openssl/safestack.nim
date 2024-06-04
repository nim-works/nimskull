import libcrypto

import stack

when defined(nimHasStyleChecks):
  {.push styleChecks: off.}

type
  STACK_OF*[T] = distinct OPENSSL_STACK
  compfunc*[T] = proc (a: ptr T, b: ptr T): cint {.cdecl.}
  copyfunc*[T] = proc (a: ptr T): ptr T {.cdecl.}
  freefunc*[T] = proc (a: ptr T) {.cdecl.}

proc new*[T](compare: compfunc[T]): ptr STACK_OF[T] {.inline.} =
  (ptr STACK_OF[T]): OPENSSL_sk_new(cast[OPENSSL_sk_compfunc](compare))
proc new_null*(T: typedesc): ptr STACK_OF[T] {.inline.} =
  (ptr STACK_OF[T]): OPENSSL_sk_new_null()
proc new_reserve*[T](c: compfunc[T], n: cint): ptr STACK_OF[T] {.inline.} =
  (ptr STACK_OF[T]): OPENSSL_sk_new_reserve(cast[OPENSSL_sk_compfunc](c), n)
proc free*[T](sk: ptr STACK_OF[T]) {.inline.} =
  OPENSSL_sk_free((ptr OPENSSL_STACK) sk)
proc pop_free*[T](sk: ptr STACK_OF[T], fn: freefunc[T]) {.inline.} =
  OPENSSL_sk_pop_free((ptr OPENSSL_STACK) sk, cast[OPENSSL_sk_freefunc](fn))
proc deep_copy*[T](sk: ptr STACK_OF[T], copyfunc: copyfunc[T], freefunc: freefunc[T]): ptr STACK_OF[T] {.inline.} =
  (ptr STACK_OF[T]): OPENSSL_sk_deep_copy((ptr OPENSSL_STACK) sk, cast[OPENSSL_sk_copyfunc](copyfunc), cast[OPENSSL_sk_freefunc](freefunc))
proc dup*[T](sk: ptr STACK_OF[T]): ptr STACK_OF[T] {.inline.} =
  (ptr STACK_OF[T]): OPENSSL_sk_deep_copy((ptr OPENSSL_STACK) sk)

proc delete*[T](sk: ptr STACK_OF[T], i: cint): ptr T {.inline.} =
  cast[ptr T](OPENSSL_sk_delete((ptr OPENSSL_STACK) sk, i))
proc delete_ptr*[T](sk: ptr STACK_OF[T], p: ptr T): ptr T {.inline.} =
  cast[ptr T](OPENSSL_sk_delete_ptr((ptr OPENSSL_STACK) sk, p))
proc find*[T](sk: ptr STACK_OF[T], data: ptr T): cint {.inline.} =
  OPENSSL_sk_find((ptr OPENSSL_STACK) sk, data)
proc find_all*[T](sk: ptr STACK_OF[T], data: ptr T, pnum: ptr cint): cint {.inline.} =
  OPENSSL_sk_find_all((ptr OPENSSL_STACK) sk, data, pnum)
proc find_ex*[T](sk: ptr STACK_OF[T], data: ptr T): cint {.inline.} =
  OPENSSL_sk_find_ex((ptr OPENSSL_STACK) sk, data)
proc insert*[T](sk: ptr STACK_OF[T], data: ptr T, idx: cint): cint {.inline.} =
  OPENSSL_sk_insert((ptr OPENSSL_STACK) sk, data, idx)
proc is_sorted*[T](sk: ptr STACK_OF[T]): cint {.inline.} =
  OPENSSL_sk_is_sorted((ptr OPENSSL_STACK) sk)
proc num*[T](sk: ptr STACK_OF[T]): cint {.inline.} =
  OPENSSL_sk_num((ptr OPENSSL_STACK) sk)
proc pop*[T](sk: ptr STACK_OF[T]): ptr T {.inline.} =
  cast[ptr T](OPENSSL_sk_pop((ptr OPENSSL_STACK) sk))
proc push*[T](sk: ptr STACK_OF[T], data: ptr T): cint {.inline.} =
  OPENSSL_sk_push((ptr OPENSSL_STACK) sk, ptr)
proc reserve*[T](sk: ptr STACK_OF[T], n: cint): cint {.inline.} =
  OPENSSL_sk_reserve((ptr OPENSSL_STACK) sk, n)
proc set*[T](sk: ptr STACK_OF[T], idx: cint, data: ptr T): ptr T {.inline.} =
  cast[ptr T](OPENSSL_sk_set((ptr OPENSSL_STACK) sk, idx, data))
proc set_cmp_func*[T](sk: ptr STACK_OF[T], cmp: compfunc[T]): compfunc[T] {.inline.} =
  cast[compfunc[T]](OPENSSL_sk_set_cmp_func((ptr OPENSSL_STACK) sk, cast[OPENSSL_sk_compfunc](cmp)))
proc shift*[T](sk: ptr STACK_OF[T]): ptr T {.inline.} =
  cast[ptr T](OPENSSL_sk_shift((ptr OPENSSL_STACK) sk))
proc sort*[T](sk: ptr STACK_OF[T]) {.inline.} =
  OPENSSL_sk_sort((ptr OPENSSL_STACK) sk)
proc unshift*[T](sk: ptr STACK_OF[T], data: ptr T): cint {.inline.} =
  OPENSSL_sk_unshift((ptr OPENSSL_STACK) sk, data)
proc value*[T](sk: ptr STACK_OF[T], i: cint): ptr T {.inline.} =
  cast[ptr T](OPENSSL_sk_value((ptr OPENSSL_STACK) sk, i))
proc zero*[T](sk: ptr STACK_OF[T]) {.inline.} =
  OPENSSL_sk_zero((ptr OPENSSL_STACK) sk)
