discard """
  description: "Regression test for internal tail call transformation bug"
"""

type Object = object
  ## a type that uses pass-by-ref internally
  arr: array[4, int]

proc a(x: Object): Object {.tailcall.} =
  x

proc b(x: sink Object): Object {.tailcall.} =
  x

# the procedure with the non-sink parameter must be used first
let ia = a
let ib = b
doAssert ia(Object(arr: [1, 2, 3, 4])).arr == [1, 2, 3, 4]
doAssert ib(Object(arr: [1, 2, 3, 4])).arr == [1, 2, 3, 4]
