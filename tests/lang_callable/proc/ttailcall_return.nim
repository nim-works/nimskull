discard """
  description: '''
    Tests for expressions used directly as a tailcall's return operand.
  '''
"""

proc tup(): (int, int) {.tailcall.} =
  (1, 2)

doAssert tup() == (1, 2)

proc arr(): array[2, int] {.tailcall.} =
  [1, 2]

doAssert arr() == [1, 2]

proc bitset(): set[uint8] {.tailcall.} =
  {1'u8, 2}

doAssert bitset() == {1'u8, 2}

type Obj = object
  a: int

proc obj(): Obj {.tailcall.} =
  Obj(a: 1)

doAssert obj() == Obj(a: 1)
