discard """
labels: "codegen error_compilation generics template"
description: '''
  . From https://github.com/nim-lang/Nim/issues/6756
    Error in template when using the type of the parameter inside it
  . The problem is in the line `var res: A[T]`. The assert fails because symbol is
    `A_105025: ["sfUsed"], [], ["issue.nim", 5, 15], skType`
  . If I skip symbols of kind skType, it works.
'''
"""

import typetraits
type
  A[T] = ref object
    v: T

template templ(o: A, op: untyped): untyped =
  type T = typeof(o.v)

  var res: A[T]

  block:
    var it {.inject.}: T
    it = o.v
    res = A[T](v: op)
  res

let a = A[int](v: 1)
let representation = $( templ(a, it + 2)[] )
doAssert representation == "(v: 3)"