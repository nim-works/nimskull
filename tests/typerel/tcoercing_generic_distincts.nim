discard """
  description: '''
    Ensure that types can be coerced into generic distinct types
  '''
  action: compile
"""

type
  Tuple[A] = (int, int) # tuple with phantom type information

  Generic[T]  = distinct T
  Generic2[T] = distinct Tuple[T]

  Generic3[T] = distinct Tuple[int]
    ## generic phantom distinct type of a phantom type

template check(val: untyped, expect: untyped) =
  static: doAssert typeof(val) is expect

var x: Tuple[float] = (0, 0)

# coercing a value to an explicitly distinct type works:
check Generic[int](0), Generic[int]
check Generic(int(0)), Generic[int]

# coercing a value that is of phantom type to a distinct type that uses the
# phantom type information works:
check Generic2[float](x), Generic2[float]
check Generic2(x),        Generic2[float]

# check that the full instantiated type is bound in the "distinct of generic
# parameter" case
check typeof(Generic(x)).T.A, float

static:
  # XXX: issue unrelated to typerel. ``checkConvertible`` skips the relevant
  #      type information before calling ``cmpTypes``
  doAssert compiles(Generic2[int](x)),
          "conversion-to-generics start working properly"

  # check that different phantom types are not treated as equal in
  # to-distinct coercions
  doAssert not compiles(Generic3(x))
