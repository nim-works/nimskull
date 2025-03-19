discard """
  description: "Temporaries cannot be passed to pass-by-reference parameters"
  action: reject
"""

type Large = object
  val: array[255, int]

proc testm(x: Large): int {.musttail.} = x.val[0]
proc test(): int =
  testm(Large())
