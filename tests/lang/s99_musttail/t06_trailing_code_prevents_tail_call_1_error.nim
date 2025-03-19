discard """
  action: reject
"""

proc p() {.musttail.} = discard

proc test() =
  p()
  echo "x"
