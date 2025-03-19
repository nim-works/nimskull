discard """
  description: ".musttail routines cannot use outer variables"
  action: reject
"""

proc outer() =
  var x = 0
  proc inner() {.musttail.} =
    x = 1
  inner()

outer()
