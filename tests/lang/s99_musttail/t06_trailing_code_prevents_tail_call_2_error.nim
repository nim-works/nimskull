discard """
  action: reject
"""

proc p(): int {.tailcall.} = 0

proc test() {.tailcall.} =
  echo p()
