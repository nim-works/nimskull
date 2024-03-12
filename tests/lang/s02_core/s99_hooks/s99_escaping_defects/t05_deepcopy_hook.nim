discard """
  description: "`=copy` hooks panic when a defect escapes"
  outputsub: "Error: unhandled exception:  [Defect]"
  exitcode: 1
"""

type Type = ref object

proc `=deepcopy`(a: Type): Type =
  raise (ref Defect)()

try:
  var x = `=deepcopy`(Type())
finally:
  # finally sections are not reached and no cleanup is performed
  doAssert false
