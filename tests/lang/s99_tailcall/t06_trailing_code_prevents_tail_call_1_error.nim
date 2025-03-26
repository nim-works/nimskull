discard """
  action: reject
"""

proc p() {.tailcall.} = discard

proc test() {.tailcall.} =
  p()
  echo "x"
