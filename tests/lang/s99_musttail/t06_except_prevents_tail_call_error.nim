discard """
  action: reject
"""

proc p() {.tailcall.} = discard

proc test() {.tailcall.} =
  try:
    discard
  except:
    p()
