discard """
  description: '''
    Ensure that a non-early-inlined const value containing an empty `seq` can
    be code evaluated with the compile-time VM
  '''
  target: native
  action: compiles
"""

type Obj = object
  x: seq[int]

const c = Obj(x: @[])
# ^^ any type that doesn't result in semantic analysis inlining the constant
# work here. The error only triggered for non-inlined constants

static:
  # using `c` in a compile-time context crashed the compiler
  var v = c
  assert v.x.len == 0