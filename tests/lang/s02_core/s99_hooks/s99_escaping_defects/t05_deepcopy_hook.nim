discard """
  description: "`=deepcopy` hooks panic when a defect escapes"
  outputsub: "Error: unhandled exception: error [Defect]"
  exitcode: 1
"""

type Type = ref object

proc `=deepcopy`(a: Type): Type =
  raise (ref Defect)(msg: "error")

try:
  var x = `=deepcopy`(Type())
finally:
  # finally sections are not reached and no cleanup is performed
  doAssert false
