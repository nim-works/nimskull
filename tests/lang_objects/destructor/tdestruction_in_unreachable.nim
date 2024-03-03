discard """
  description: '''
    Ensure that no destructor is called for a local defined in an unreachable
    part of the code, even if the scope is left via unstructured control-flow.
  '''
  targets: "c js vm"
"""

type Object = object

var wasDestroyed = false

proc `=destroy`(x: var Object) =
  wasDestroyed = true

proc test(cond: bool) =
  block:
    if cond:
      return
    else:
      raise CatchableError.newException("")

  # everything beyond this point is statically unreachable code
  var o = Object()
  if cond:
    # introduce an unstructured exit of the scope
    return

  discard o

test(true)

# XXX: wasDestroy must be false, but it currently isn't. Testing the inverse
#      makes sure that the test at least compiles
doAssert wasDestroyed, "the behaviour is correct now"