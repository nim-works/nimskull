discard """
  output: '''
1
2
3
4
5
caught: a
got: 1
caught: cannot tail call
'''
"""

# XXX: consider splitting this test into multiple files

## The coroutine instance passed to ``tail`` doesn't have to be in the
## initial, never-ran state.

proc a() {.coroutine.} =
  echo "2"
  suspend()
  echo "4"

proc b() {.coroutine.} =
  echo "1"
  var coro = a()
  resume(coro)
  echo "3"
  tail(coro)
  echo "5"

resume(b())

## It also allowed to tail-call pending or aborted instances.

proc c(doRaise: bool): int {.coroutine.} =
  if doRaise:
    raise CatchableError.newException("a")
  else:
    return 1

proc d() {.coroutine.} =
  var coro = c(true)
  resume(coro)
  try:
    discard tail(coro)
  except CatchableError as e:
    echo "caught: ", e.msg

  coro = c(false)
  resume(coro)
  echo "got: ", tail(coro)

resume(d())

## It is not allowed to tail-call a finished or running instance. On doing so,
## a ``CoroutineError`` exception is raised.

proc e() {.coroutine.} =
  try:
    tail(e)
  except CoroutineError as e:
    echo "caught: ", e.msg # already running

resume(e())

proc f() {.coroutine.} =
  discard "returns immediately"

proc g() {.coroutine.} =
  var coro = f()
  resume(coro)
  finish(coro)
  try:
    tail(coro)
  except CoroutineError as e:
    echo "caught: ", e.msg

resume(g())
