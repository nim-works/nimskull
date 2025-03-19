discard """
  action: reject
"""

proc p() {.musttail.} = discard

method m(x: RootRef) =
  p()
