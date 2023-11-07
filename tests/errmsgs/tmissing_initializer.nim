discard """
  errormsg: "The type 'Obj' requires an initial value"
  line: 8
"""

type Obj {.requiresInit.} = object

var x: Obj