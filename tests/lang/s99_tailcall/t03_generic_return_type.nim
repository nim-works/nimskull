discard """
  description: "The return type of a .tailcall routine may be generic."
"""

proc test[T](x: T): T {.tailcall.} = x

doAssert test(1) == 1
