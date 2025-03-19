discard """
  action: reject
"""

proc p() {.musttail.} = discard

proc m() {.exportc.} =
  p()
