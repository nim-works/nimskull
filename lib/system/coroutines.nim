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
    next: CoroutineBase
      ## the instance to continue with (i.e., the *continuation*) once
      ## control-flow leaves the current coroutine
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

proc suspend(next: sink CoroutineBase) {.magic: "Suspend".}
  ## Internal-only suspend. Sets the instance returned by the coroutine
  ## procedure to `next`

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
  ## then executed until the first suspension point is reached. Returns
  ## the coroutine instance in which a suspension point was reached.
  if c.state >= 0:
    # already running
    discard "TODO: raise an error"

  # run the coroutine and its continuations until a suspension point is
  # reached:
  var c = c
  while true:
    if c.state <= -StateOffset:
      # mark as running:
      c.state = -(c.state + StateOffset)

    var next: CoroutineBase
    try:
      next = c.fn(c)
    except CatchableError as e:
      c.state = -2 # aborted
      c.exc = e
      next = c.next

    if next.isNil:
      break # nothing to continue with
    c = next

  # mark the exit instance as suspended, if not pending or aborted:
  if c.state >= 0:
    c.state = -(c.state + StateOffset)

  result = c

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

proc getOrRaise[T](c: Coroutine[T]): T =
  case c.status
  of csAborted:
    c.state = -3
    raise move(c.exc)
  of csPending:
    when T isnot void:
      result = move c.result
    c.state = -3
  else:
    raise CoroutineError.newException("not pending")

template tail*[T](c: Coroutine[T]): untyped =
  ## Passes control to coroutine instance `c`. When `c` logically returns,
  ## control is passed back to the currently running coroutine, without
  ## original `resume <#resume,CoroutineBase>`_ returning.
  # TODO: move the checks and setup into a separate procedure
  let x = c
  case x.status
  of csRunning:
    raise CoroutineError.newException("tail-call target already running")
  of csSuspended:
    # XXX: an error is not strictly necessary, we could also take over the
    #      continuation
    if x.next != nil:
      raise CoroutineError.newException("cannot tail-call again")
    x.next = self
    suspend(x) # pass control to the coroutine
  else:
    discard "okay, do nothing"
  getOrRaise(x)

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
