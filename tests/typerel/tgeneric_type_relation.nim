discard """
  action: compile
  description: "Specification tests for the relationship between generic types"
"""

type
  GenericA[T] = (T, T)
  GenericB[T] = (T, T)

static:
  # generic types use name equivalence: they are equal if they have the same
  # *name* and their parameters are equal:
  doAssert GenericA[int] is GenericA[int]
  doAssert GenericA[int] isnot GenericA[float]
  # all instances of a generic type are part of the implicit type-class
  # covering all instances of the generic type:
  doAssert GenericA[int] is GenericA

  # if the name is different, the types are not equal:
  doAssert GenericA[int] isnot GenericB[int]
  doAssert GenericA[int] isnot GenericB