discard """
  description: '''
    Invoking a generic range type with a ``static`` normal parameter instead
    of a generic parameter must work
  '''
  knownIssue: '''
    The type variable in the range expression reaches the final
    ``semtypinst.prepareNode`` call as something that is not detected as a
    type variables
  '''
"""

type Range[N: static int] = range[0 .. (N + 2)]

proc f(A: static int, rng: Range[A]): int =
  result = typeof(rng).high.int

# test with non-range:
doAssert f(3, 0) == 5
# test with range:
var a: range[7..10]
doAssert f(3, a) == 8
# negative test: make sure that range types not overlapping are rejected
doAssert not compiles(f(3, b) == 5)
