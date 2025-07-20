discard """
  description: '''
    Implementation-independent tests for copy/move/destruction behaviour in
    and around suspend blocks.
  '''
"""

type Copyable = object

var numCopies = 0

proc `=copy`(a: var Copyable, b: Copyable) =
  inc numCopies

proc use[T](x: T) = discard

proc pass[C, V, R](
    x: sink V, c: sink (C, proc(p: sink V, c: sink C): R {.nimcall.})): R =
  c[1](x, c[0])

# ---- tests for copying locals into suspend blocks

proc copyIntoSuspend() =
  var x = Copyable()
  let got = suspend(Copyable, cont, pass(x, cont))
  use(x) # use after resume

copyIntoSuspend()
doAssert numCopies == 1

proc copyResultIntoSuspend(): Copyable =
  let got = suspend(Copyable, cont, pass(result, cont))
  use(result) # use after resume

numCopies = 0
discard copyResultIntoSuspend()
doAssert numCopies == 1

proc copySinkParamIntoSuspend(x: sink Copyable) =
  let got = suspend(Copyable, cont, pass(x, cont))
  use(x) # use after resume

numCopies = 0
copySinkParamIntoSuspend(Copyable())
doAssert numCopies == 1
