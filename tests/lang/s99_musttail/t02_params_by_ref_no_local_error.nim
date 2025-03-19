discard """
  description: "Locals cannot be passed to pass-by-reference parameters"
  action: reject
"""

type Large = array[255, int]

proc testm(x: Large): int {.musttail.} = x[0]
proc test(): int =
  var x: Large
  testm(x)
