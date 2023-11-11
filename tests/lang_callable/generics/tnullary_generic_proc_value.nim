discard """
  description: '''
    Ensure that nullary generic procedures can be explicitly instantiated and
    the result treated as a first-class procedural value
  '''
"""

proc nullary[](x: int): int =
  result = x

let p = nullary[]
doAssert p(1) == 1