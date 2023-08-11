discard """
  targets: "c js vm"
  description: '''
    Tests for indirect calls where the callee is an expression evaluating to a
    view
  '''
  knownIssue.vm: '''
    Semantic analysis produces incorrect AST for tuple initialization, causing
    VM access violation errors at run-time
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
  # - a bracket expression
  # - a parenthesized expression
  type Obj = object
    x: lent Proc
    tup: (lent Proc,)

  let
    p = proc (x: int): int = x
    o = Obj(x: p, tup: (p,))

  doAssert o.x(2) == 2
  doAssert (o.x)(3) == 3
  # XXX: semantic analysis currently generates invalid code tuple or array
  #      constructors where an element is a view, so the below would crash at
  #      run-time
  doAssert compiles(o.tup[0](4) == 4)