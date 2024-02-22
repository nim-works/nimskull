discard """
  description: '''
    Regression test for a bug where manually invoking a type's `=destroy` hook
    within a procedure defined as part of a typed macro/template argument
    crashed the compiler
  '''
"""

type Destroy = object
  ## A type that has a user-defined destroy hook.

proc `=destroy`(x: var Destroy) =
  discard

type
  NoHookObject = object
    ## Object type where no destroy hook needs to be synthesized.
  HookObject = object
    ## Object type that needs a compiler-synthesized destroy hook.
    field: Destroy

macro m(x: typed): untyped = x

# the compiler crashed when processing the macro output
m:
  proc f() =
    # the bug only surfaced when the hook call is part of a procedure. Types
    # that need a compiler-synthesized as well as those that don't were
    # affected.
    var v = NoHookObject()
    `=destroy`(v)
    var v2 = HookObject()
    `=destroy`(v2)

# the same bug happend when using a template:
template t(x: typed): untyped = x

t:
  proc f2() =
    var v = NoHookObject()
    `=destroy`(v)
    var v2 = HookObject()
    `=destroy`(v2)