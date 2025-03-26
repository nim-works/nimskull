discard """
  description: ".tailcall routines cannot use outer variables"
  action: reject
"""

proc outer() =
  var x = 0
  proc inner() {.tailcall.} =
    x = 1
  inner()

outer()
