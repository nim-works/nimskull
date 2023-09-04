discard """
  errormsg: "'Generic' is not a concrete type"
  line: 8
"""

type
  NonGeneric = object
    field: Generic

  Generic[T] = object