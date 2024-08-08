discard """
  description: "`=destroy` hooks panic when a defect escapes"
  outputsub: "Error: unhandled exception: error [Defect]"
  exitcode: 1
"""

type Type = object

proc `=destroy`(a: var Type) =
  raise (ref Defect)(msg: "error")

try:
  var x: Type
  `=destroy`(x)
finally:
  # finally sections are not reached and no cleanup is performed
  doAssert false
