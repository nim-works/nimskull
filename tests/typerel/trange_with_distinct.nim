discard """
  knownIssue: '''
    A range type with a distinct base matches equally well against both
    ``range`` and ``distinct``, which leads to ambiguity errors
  '''
"""

type DInt = distinct int

proc p[T: range](x: T): int =
  result = 1

proc p[T: distinct](x: T): int =
  result = 2

var x: range[DInt(0)..DInt(1)]
# the overload using the ``range``-constraint should match better than the one
# with the ``distinct``
doAssert p(x) == 1