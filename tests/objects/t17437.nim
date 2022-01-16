discard """
  cmd: "nim check --hints:off $file"
  action: reject
  nimout: '''
t17437.nim(20, 16) Error: undeclared identifier: 'x'
t17437.nim(20, 16) Error: expression has no type: x
t17437.nim(20, 19) Error: Invalid field assignment 'y'
t17437.nim(20, 12) Error: Invalid object constructor: 'V(x: x, y)'
t17437.nim(20, 12) Error: expression has no type: V(x: x, y)
'''
"""

# bug #17437 invalid object construction should result in error

type
  V = ref object
    x, y: int

proc m =
  var v = V(x: x, y)

m()
