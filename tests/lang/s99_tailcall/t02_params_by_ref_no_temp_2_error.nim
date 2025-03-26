discard """
  description: "Temporaries cannot be passed to pass-by-reference parameters"
  action: reject
"""

type Large = array[255, int]

proc p(x: Large): int {.tailcall.} = x[0]
proc test(): int {.tailcall.} =
  p(default(Large))
