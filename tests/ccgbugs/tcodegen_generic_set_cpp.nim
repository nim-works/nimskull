discard """
labels: "array codegen error_compile generics seq"
description: '''
  . From https://github.com/nim-lang/Nim/issues/8967
    marshal.nim can't be compiled for the cpp backend
  . Sometimes sets are materialized as arrays and we must treat them as
    such: the CPP backend is pickier than the C one and would sometime
    produce invalid code.
'''
"""

import marshal

let orig: set[char] = {'A'..'Z'}
let m = $$orig
let old = to[set[char]](m)
doAssert orig - old == {}