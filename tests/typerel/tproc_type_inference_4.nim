discard """
  description: '''
    A generic parameter can be inferred as `void` from procedure type's return
    type
  '''
"""

proc test[T](x: proc(): T) =
  doAssert T is void
  x()

proc voidProc() = discard

# type parameter inference works
test(voidProc)
# also works with an explicit type
test[void](voidProc)
