discard """
  description: "The return type of a .tailcall proc type may be generic."
"""

type Generic[T] = proc(x: T): T {.tailcall.}

proc test(x: int): int {.tailcall.} = x

var x: Generic[int]
x = test
doAssert x(1) == 1
