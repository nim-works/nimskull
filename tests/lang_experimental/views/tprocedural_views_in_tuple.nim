discard """
  description: '''
    Ensure views of procedural values can be stored in and called from tuples
    via the bracket syntax
  '''
  targets: "c js vm"
  knownIssue.c vm: '''
    Semantic analysis produces incorrect AST for tuple initialization
  '''
"""

# tracked by https://github.com/nim-works/nimskull/issues/1376

# note: the JavaScript target is affected too, but the bug doesn't lead to any
# compile or run-time errors

{.experimental: "views".}

type Proc = proc(x: int): int {.nimcall.}

proc test() =
  # wrap the test in a procedure to make sure that local variables are used
  let
    p = proc (x: int): int = x
    tup: (lent Proc,) = (p,)

  doAssert tup[0](2) == 2

test()
