discard """
  description: '''
    Ensure that `getType` and friends work for all type class of built-in
    types.
  '''
  action: compile
"""

import std/macros

macro test(x: typed) =
  # try all ``getType`` procedures
  discard getType(x)
  discard getTypeImpl(x)
  discard getTypeInst(x)

var x: int

#test(object)
test(distinct)
test(enum)
test(Ordinal)
test(array)
test(tuple)
test(set)
test(range)
test(ptr)
test(ref)
test(seq)
test(proc)
test(var)
test(lent)
test(openArray)
test(varargs)
test(UncheckedArray)