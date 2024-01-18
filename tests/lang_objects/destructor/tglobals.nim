discard """
  description: "Tests for globals that have lifetime hooks attached"
  targets: "c js vm"
  matrix: "--cursorInference:off"
"""

import mhelper except test

numCopies = 0
numDestroy = 0
block move_non_module_scope_global:
  var x = initResource()
  var y = x # move -- this is the last use of `x`

doAssert numCopies == 0
doAssert numDestroy == 1

numCopies = 0
numDestroy = 0
block consider_call_with_side_effects:
  # for this test, it's important for both variables to not be located at
  # module-scope but also not inside a procedure
  var x = initResource()
  var y = x # must copy...

  proc p() =
    mutate(x)

  p() # <-- because `x` is mutated here

doAssert numCopies == 1
doAssert numDestroy == 2

block missing_default_init_at_scope_start:
  # regression test: globals in nested scopes were not default-initialized
  # at the start of their scope, which lead to an old state being
  # observable within loop cleanup
  type Obj = object
    isDestroyed: bool

  proc `=destroy`(x: var Obj) =
    doAssert not x.isDestroyed, "an old state is observed"
    x.isDestroyed = true

  proc cond(): bool =
    # hide the value in order to prevent folding
    false

  var i = 0
  # don't use a non-true loop condition so that control-flow and scopes are
  # explicit
  while true:
    if i == 1:
      break

    let o = Obj(isDestroyed: false)
    if cond(): # never actually entered
      # the break (i.e., unstructured exit) here is necessary to force the
      # above `break` to trigger `o`'s destructor
      # XXX: this is the current behaviour, but it's incorrect
      break

    doAssert not o.isDestroyed
    inc i
