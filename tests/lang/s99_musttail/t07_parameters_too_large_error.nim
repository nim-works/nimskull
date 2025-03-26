discard """
  reject: true
"""

type Large = array[1024, int]

proc p(x: sink Large) {.musttail.} =
  discard
