discard """
  description: '''
    Ensure that a discriminator assignment doesn't prevent automatic moves
    from fields outside the record-case
  '''
  action: compile
  matrix: "--expandArc:test --checks:off"
  nimout: '''--expandArc: test
scope:
  def _3: WithHooks = ()
  def x: Object = (a: consume _3, kind: consume true)
  bind_mut _4: WithHooks = x.a
  result := move _4
  wasMoved(name _4)
  bind_mut _5: bool = x.kind
  def _6: bool = copy kind
  def _7: bool = eqB(arg _5, arg _6)
  def _8: bool = not(arg _7)
  if _8:
    =destroy(name x)
  _5 = _6
  =destroy(name x)

-- end of expandArc ------------------------'''
"""

type
  WithHooks = object
  Object = object
    a: WithHooks
    case kind: bool
    of true, false:
      b: WithHooks

proc `=destroy`(x: var WithHooks) =
  discard

proc test(kind: bool): WithHooks {.exportc.} =
  var x = Object(a: WithHooks(), kind: true)
  result = x.a # can be moved
  # the discriminator assignment must not prevent `x.a` from being moved out
  # of
  x.kind = kind
