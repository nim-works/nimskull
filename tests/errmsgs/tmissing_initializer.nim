discard """
  errormsg: "The type 'Obj' doesn't have a default value"
  line: 8
"""

type Obj {.requiresInit.} = object

var x: Obj