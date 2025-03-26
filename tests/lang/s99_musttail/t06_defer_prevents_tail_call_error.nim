discard """
  action: reject
"""

proc p() {.tailcall.} = discard

proc test() {.tailcall.} =
  defer: echo ""
  p()
