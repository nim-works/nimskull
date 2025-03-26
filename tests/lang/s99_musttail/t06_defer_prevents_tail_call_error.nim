discard """
  action: reject
"""

proc p() {.musttail.} = discard

proc test() {.musttail.} =
  defer: echo ""
  p()
