discard """
  action: reject
"""

proc p() {.tailcall.} = discard

proc test() {.tailcall.} =
  try:
    p()
  finally:
    discard
