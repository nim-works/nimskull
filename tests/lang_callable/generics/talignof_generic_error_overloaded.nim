discard """
  errormsg: "'GenericType' is not a concrete type"
  line: 13
"""

type
  GenericType[K, V] = object
    field: (K, V)

# Overload alignof
proc alignof(a, b: int) = discard

let alignment = alignof(GenericType)
