discard """
  errormsg: "case statement cannot work on enums with holes for computed goto"
  line: 21
  description: '''
    . From ComputedGoto: bad codegen with enum with holes
    . There are two possible solutions:
      Raise an error if the enum has holes
      Complicate the codegen a bit by generating a set[uint16]
      of the enum coverage and then use it to generate the jump tables.
    . IMHO, error is the way to go. If you want speed, design your enums properly.
  '''
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