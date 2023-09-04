discard """
  errormsg: "'any' is not a concrete type"
  line: 13
"""

# a contrived case: a non-generic recursive object type containing a meta-type
# field is used in a generic object that is instantiated before the final type-
# section pass is run

type
  NonGeneric = ref object
    self: NonGeneric
    field: any

  Generic[T] = object
    x: NonGeneric

  NonGeneric2 = object
    x: Generic[int]
