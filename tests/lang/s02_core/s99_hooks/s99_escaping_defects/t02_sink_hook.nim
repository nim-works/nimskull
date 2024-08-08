discard """
  description: "`=sink` hooks panic when a defect escapes"
  outputsub: "Error: unhandled exception: error [Defect]"
  exitcode: 1
"""

type Type = object

proc `=sink`(a: var Type, b: Type) =
  raise (ref Defect)(msg: "error")

try:
  var x: Type
  `=sink`(x, Type())
finally:
  # finally sections are not reached and no cleanup is performed
  doAssert false
