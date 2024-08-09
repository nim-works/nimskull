discard """
  targets: "c js vm"
  description: '''
    Tests for indirect calls where the callee is an expression evaluating to a
    view
  '''
"""

{.experimental: "views".}

type Proc = proc(x: int): int {.nimcall.}

block direct_view:
  # the callee is view stored in a global
  let p = proc (x: int): int = x
  let view: lent Proc = p

  doAssert view(1) == 1

block complex_view_callee:
  # the callee expression evaluate to a view is a:
  # - dot expression
  # - a parenthesized expression
  type Obj = object
    x: lent Proc

  let
    p = proc (x: int): int = x
    o = Obj(x: p)

  doAssert o.x(2) == 2
  doAssert (o.x)(3) == 3
