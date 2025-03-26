discard """
  description: "Temporaries cannot be passed to pass-by-reference parameters"
  action: reject
"""

type Large = object
  val: array[255, int]

proc p(x: Large): int {.tailcall.} = x.val[0]
proc test(): int {.tailcall.} =
  p(Large())
