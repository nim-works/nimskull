discard """
  description: '''
    Ensure that instantiating a generic distinct type where the generic
    parameter is not used in the body yields distinct types for different
    parameters
  '''
  action: compile
"""

type
  TypeA[T] = distinct T
  TypeB[T] = distinct int

static:
  doAssert TypeA[int] is    TypeA[int]
  # make sure two separate generic types result in distinct instances:
  doAssert TypeA[int] isnot TypeB[int]
  doAssert TypeB[int] isnot TypeA[int]

  # try with a different parameter type:
  doAssert TypeA[float] is    TypeA[float]
  doAssert TypeA[float] isnot TypeB[float]
  doAssert TypeB[float] isnot TypeA[float]

  # ensure that different paramaters result in different types:
  doAssert TypeA[int] isnot TypeA[float]
