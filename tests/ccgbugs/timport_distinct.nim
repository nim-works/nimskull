discard """
  description: '''
    Regression test for importing ``distinct`` types resulting in duplicate C
    type definitions
  '''
"""

type
  Object = object
  Imported {.importc: "int", nodecl.} = distinct Object

var x: Object
var y: Imported

# two type definitions for `Object` were emitted
