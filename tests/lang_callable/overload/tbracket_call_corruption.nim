discard """
  description: "Regression test for `[]` calls causing AST corruption"
"""

proc op(x: int) =
  discard

template overload(x: untyped) =
  # this is the overload that is picked. Its definition has to come
  # first, so that it's processed first during overload resolution
  op(x) # this failed with an "expected `int` but got `T`" error

template overload(x: bool) =
  # an overload that must not match
  discard

# test with an explicit bracket call:
overload(`[]`([0], 0))

# test with an implicit bracket call in a generic:
proc f[T]() =
  overload([0][0])

f[int]()

# test with an implicit bracket call in a template:
template t() =
  overload([0][0])

t()