discard """
  action: reject
"""

proc p() {.musttail.} = discard

proc test() {.musttail.} =
  p()
  echo "x"
