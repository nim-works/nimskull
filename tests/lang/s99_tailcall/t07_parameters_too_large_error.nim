discard """
  action: reject
"""

type Large = array[1024, int]

proc p(x: sink Large) {.tailcall.} =
  discard
