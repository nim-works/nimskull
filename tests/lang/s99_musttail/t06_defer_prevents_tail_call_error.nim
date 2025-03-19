discard """
  action: reject
"""

proc p() {.musttail.} = discard

proc test() =
  defer: echo ""
  p()
