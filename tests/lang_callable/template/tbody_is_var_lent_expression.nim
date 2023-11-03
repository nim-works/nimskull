discard """
  description: '''
    Regression test for the case where the body of a template/macro with a
    concrete return type is of `lent`/`var` tuple type resulted in wrong code
    being generated
  '''
"""

# the tuple type's content doesn't matter
type Tuple = tuple[x: int]

proc getVar(x: var Tuple): var Tuple = x
proc getLent(x: Tuple): lent Tuple = x


template templVar(x: Tuple): Tuple =
  # using ``untyped`` as the return type would have the code work
  # properly. The body was not properly typed, resulting in an implicit
  # conversion from ``lent Tuple`` to ``Tuple`` being inserted by the
  # compiler
  getVar(x)

template templLent(x: Tuple): Tuple =
  getLent(x)

var tup = (x: 1)

let v = templLent(tup)
doAssert v.x == 1

let v2 = templVar(tup)
doAssert v.x == 1