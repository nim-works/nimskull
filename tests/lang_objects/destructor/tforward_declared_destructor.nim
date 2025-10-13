discard """
  description: '''
    Regression test for a forward-declared destroy hook causing a compiler
    crash when hooks are requested before the forward-declaration is
    completed
  '''
"""

# note: the comments refer to implementation details that were present at the
# time of writing

type
  Object* = object

# it's important that the destroy hook is forward-declared here
proc `=destroy`(x: var Object)

# use `Object` in a way so that no hooks are requested before `p` is
# instantiated
proc test(x: Object) =
  # request hooks for the type with the still-forward-declared destroy hook,
  # resulting in a compiler crash
  var x = x

# finish the forward declaration
proc `=destroy`(x: var Object) =
  discard
