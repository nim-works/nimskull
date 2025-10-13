discard """
  errormsg: "'GenericType' is not a concrete type"
  line: 10
"""

type
  GenericType[K, V] = object
    field: (K, V)

let size = sizeof(GenericType)
