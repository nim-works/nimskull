## Internal module included by the system module. Implements the basic
## compiler interface for coroutines, plus a low-level API for working
## with them.

type
  CoroutineBase* {.compilerproc.} = ref object of RootObj
    ## The internal base type of all coroutine instance types. Carries the
    ## state all coroutine instances have.
    fn: proc(c: CoroutineBase): CoroutineBase {.nimcall.}
      ## internal procedure
    state: int32
      ## internal state value
    exc: ref Exception
      ## the "current exception" of the coroutine instance

  Coroutine*[T] {.compilerproc.} = ref object of CoroutineBase
    ## The public base type of all coroutine instance types.
    when T isnot void:
      result: T

  CoroutineState* = enum
    ## Identifies the status of a coroutine instance.
    csSuspended
    csRunning
    csAborted
    csPending
    csFinished

  CoroutineError* = object of ValueError
    # XXX: derive from ``CatchableError`` directly?

const
  StateOffset = 4
    ## the number of special values `state` can have + 1

# compiler interface
# ------------------

proc suspend*() {.magic: "Suspend".}
  ## Suspend the active coroutine, yielding control back to the callsite of
  ## the `resume <#resume,CoroutineBase>` that earlier resumed it. The resume
  ## call will return the same instance it was invoked on.

proc suspend*(next: CoroutineBase) {.magic: "Suspend".}
  ## Suspend the active coroutine, yielding control back to the callsite of
  ## the `resume <#resume,CoroutineBase>` that earlier resumed it. `next` is
  ## the coroutine instance resume will returned.

# low-level API
# -------------

proc status*(c: CoroutineBase): CoroutineState =
  case c.state
  of -3:
    csFinished
  of -2:
    csAborted
  of -1:
    csPending
  elif c.state >= 0:
    csRunning
  else:
    csSuspended

{.push checks: off.}

proc resume*(c: CoroutineBase): CoroutineBase {.discardable.} =
  ## Yields control to the given suspendend coroutine instance `c`, which is
  ## then executed until the first suspension point is reached. Returns the
  ## coroutine instance that the coroutine yielded control to (may be nil).
  if c.state <= -StateOffset:
    # mark as running:
    c.state = -(c.state + StateOffset)
  else:
    # already running
    discard "TODO: raise an error"
  # execute until the first suspension point is reached:
  try:
    result = c.fn(c)
  except CatchableError as e:
    c.state = -2 # aborted
    c.exc = e
    result = c

  # mark as not running again:
  if c.state >= 0:
    c.state = -(c.state + StateOffset)

{.pop.}

proc finish*[T](c: sink Coroutine[T]): T =
  case c.status
  of csPending:
    when T isnot void:
      result = move c.result
    c.state = -3 # done
  else:
    raise CoroutineError.newException("not pending")

proc unwrap*(c: sink CoroutineBase): ref Exception =
  # XXX: unwrap is not a descriptive name for what the procedure does
  case c.status
  of csAborted:
    result = move c.exc
    c.state = -3
  else:
    raise CoroutineError.newException("not aborted")

proc trampoline*[T](c: sink Coroutine[T]): T =
  ## Runs the instance until control is yielded to a not-suspended instance.
  ## For an instance aborted with an exception, the exception is re-raised,
  ## otherwise the result (if any) is extracted via ``finish``.
  # XXX: unrelated to the coroutine compiler interface. This should either be
  #      directly in the system module, or in a separate standard library
  #      module.
  var c = CoroutineBase(c)
  while c != nil and c.status == csSuspended:
    c = c.resume()

  if c.isNil:
    raise CoroutineError.newException("coroutine was dismissed")

  case c.status
  of csSuspended:
    doAssert false # not possible
  of csAborted:
    # raise for
    raise c.exc
  of csPending:
    when T is void:
      finish(Coroutine[T](c))
    else:
      result = finish(Coroutine[T](c))
  of csRunning:
    # the coroutine must have yielded to a another, running coroutine
    # TODO: report an error
    discard
  of csFinished:
    # the coroutine must have yielded to a finished coroutine
    # TODO: report an error
    discard
