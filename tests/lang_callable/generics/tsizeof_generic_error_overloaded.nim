discard """
  errormsg: "'GenericType' is not a concrete type"
  line: 13
"""

type
  GenericType[K, V] = object
    field: (K, V)

# Overload sizeof
proc sizeof(a, b: int) = discard

let size = sizeof(GenericType)
