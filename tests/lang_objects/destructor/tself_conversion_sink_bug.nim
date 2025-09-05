discard """
  description: '''
    Regression test for self-conversion where the source type is `sink`-
    modified causing a double-free.
  '''
"""

type Object = object
  val: int

var wasDestroyed = false

proc `=destroy`(x: var Object) =
  if x.val == 1:
    doAssert not wasDestroyed
    wasDestroyed = true

proc get(x: sink Object): Object =
  return Object(x) # pointless conversion to self

block:
  discard get(Object(val: 1))

doAssert wasDestroyed
