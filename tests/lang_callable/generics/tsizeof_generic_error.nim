discard """
  errormsg: "'GenericType' is not a concrete type"
"""

type
  GenericType[K, V] = object
    field: (K, V)

let size = sizeof(GenericType)
