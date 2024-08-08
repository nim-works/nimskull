discard """
  description: "`=trace` hooks panic when a defect escapes"
  outputsub: "Error: unhandled exception: error [Defect]"
  exitcode: 1
"""

type Type = object

proc `=trace`(a: var Type, p: pointer) =
  raise (ref Defect)(msg: "error")

try:
  var x: Type
  `=trace`(x, nil)
finally:
  # finally sections are not reached and no cleanup is performed
  doAssert false
