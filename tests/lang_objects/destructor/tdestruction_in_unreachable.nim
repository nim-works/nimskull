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

# ------------------------------
# test without if/else statement

proc test2(cond: bool) =
  block:
    # the return is within its own scope. Using a raise would have the same
    # compile-time effect, but would result in an unhandled exception at
    # run-time
    return

  var o = Object()
  if cond:
    # introduce an unstructured exit of the scope (which currently
    # forces destruction within a finally)
    return
  discard o

test2(true)
# XXX: same comment as for the assertion above
doAssert wasDestroyed, "the behaviour is correct now"