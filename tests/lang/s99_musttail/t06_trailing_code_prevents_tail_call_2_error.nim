discard """
  action: reject
"""

proc p(): int {.musttail.} = 0

proc test() =
  echo p()
