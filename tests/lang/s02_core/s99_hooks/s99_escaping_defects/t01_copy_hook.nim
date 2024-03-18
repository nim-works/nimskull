discard """
  description: "`=copy` hooks panic when a defect escapes"
  outputsub: "Error: unhandled exception: error [Defect]"
  exitcode: 1
"""

type Type = object

proc `=copy`(a: var Type, b: Type) =
  raise (ref Defect)(msg: "error")

try:
  var x: Type
  `=copy`(x, Type())
finally:
  # finally sections are not reached and no cleanup is performed
  doAssert false
