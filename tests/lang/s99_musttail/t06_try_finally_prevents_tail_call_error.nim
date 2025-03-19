discard """
  action: reject
"""

proc p() {.musttail.} = discard

proc test() =
  try:
    p()
  finally:
    discard
