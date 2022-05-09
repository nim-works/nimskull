discard """
  errormsg: "case statement cannot work on enums with holes for computed goto"
  description: '''`computedGoto` pragma currently doesn't work with enums with
holes. This seems wrong, enums with holes should be able to have successors and
increment/decrement being based on value instead of position might be the real
issue.
'''
  line: 19
  targets: "c cpp"
"""

type
  X = enum
    A = 0, B = 100

var z = A
while true:
  {.computedGoto.}
  case z
  of A: discard
  of B: discard
