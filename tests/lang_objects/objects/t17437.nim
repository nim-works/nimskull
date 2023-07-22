discard """
  cmd: "nim check --hints:off $file"
  action: reject
  nimout: '''
t17437.nim(18, 16) Error: undeclared identifier: 'x'
t17437.nim(18, 19) Error: Invalid field assignment 'y'
'''
"""

# bug #17437 invalid object construction should result in error

type
  V = ref object
    x, y: int

proc m =
  var v = V(x: x, y)

m()
