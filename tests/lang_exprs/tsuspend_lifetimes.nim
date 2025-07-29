discard """
  description: '''
    Implementation-independent tests for copy/move/destruction behaviour in
    and around suspend blocks.
  '''
"""

type Object = object
  has: bool

var numDestroy = 0
var numCopies = 0

proc `=destroy`(a: var Object) =
  if a.has:
    inc numDestroy

proc `=copy`(a: var Object, b: Object) =
  `=destroy`(a)
  a.has = b.has
  inc numCopies

proc use[T](x: T) = discard

proc pass[C, V, R](
    x: sink V, c: sink (C, proc(p: sink V, c: sink C): R {.tailcall.})): R =
  c[1](x, c[0])

# ---- tests for copying locals into suspend blocks

proc copyIntoSuspend() =
  var x = Object()
  let got = suspend(Object, cont, pass(x, cont))
  use(x) # use after resume

copyIntoSuspend()
doAssert numCopies == 1

proc copyResultIntoSuspend(): Object =
  let got = suspend(Object, cont, pass(result, cont))
  use(result) # use after resume

numCopies = 0
discard copyResultIntoSuspend()
doAssert numCopies == 2

proc copySinkParamIntoSuspend(x: sink Object) =
  let got = suspend(Object, cont, pass(x, cont))
  use(x) # use after resume

numCopies = 0
copySinkParamIntoSuspend(Object())
doAssert numCopies == 1

# ---- destruction

# locals captured by the suspend block are cleaned up when it's exited

proc cleanupInSuspend_1() =
  var v = Object(has: true)
  suspend void, cont:
    # `v` is moved into the suspend block
    use(v)

numDestroy = 0
numCopies = 0
cleanupInSuspend_1()
doAssert numDestroy == 1
doAssert numCopies == 0

proc cleanupInSuspend_2() =
  var v = Object(has: true)
  suspend void, cont:
    # `v` is copied into the suspend block
    use(v)
  use(v)

numDestroy = 0
numCopies = 0
cleanupInSuspend_2()
doAssert numDestroy == 2
doAssert numCopies == 1

# the result variable is cleaned up when raising and not handling an exception
# in the suspend block

proc raiseInExit(): Object =
  suspend void, cont:
    result = Object(has: true)
    raise ValueError.newException("")

numDestroy = 0
doAssertRaises ValueError:
  discard raiseInExit()
doAssert numDestroy == 1
