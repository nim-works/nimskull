discard """
  action: reject
"""

proc p() {.musttail.} = discard

proc test() {.musttail.} =
  try:
    p()
  finally:
    discard
