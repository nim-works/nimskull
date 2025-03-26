discard """
  description: "Temporaries cannot be passed to pass-by-reference parameters"
  action: reject
"""

type Large = array[255, int]

proc p(x: Large): int {.musttail.} = x[0]
proc test(): int {.musttail.} =
  p(default(Large))
