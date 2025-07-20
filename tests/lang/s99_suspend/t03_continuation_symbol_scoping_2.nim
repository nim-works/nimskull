discard """
  description: "A new scope is opened for the suspend block."
  action: reject
"""

proc test() =
  suspend void, cont:
    var x = 0
  x = 1 # 'x' is not visible
