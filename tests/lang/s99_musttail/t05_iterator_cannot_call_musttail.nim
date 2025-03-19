discard """
  action: reject
"""

proc p() {.musttail.} = discard

iterator iter() =
  p()
