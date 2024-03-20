## Internal module included by the system module. Implements the basic
## compiler interface for coroutines, plus a low-level API for working
## with them.

type
  CoroutineBase {.compilerproc.} = ref object of RootObj
    ## The internal base type of all coroutine instance type. Carries the
    ## state all coroutine instances have.
    fn: proc(c: CoroutineBase): CoroutineBase {.nimcall, raises: [].}
      ## internal procedure
    state: int32
      ## internal state value
    running: bool
      # XXX: interim solution, needs some more thought
    exc: Exception
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

# compiler interface
# ------------------

proc launch*(constr: untyped): auto {.noSideEffect, magic: "Launch".}
  ## Given a literal coroutine instance construction expression, constructs
  ## the instance and returns it.

proc suspend*(c: Coroutine) {.magic: "Suspend".}
  ## Suspends the enclosing coroutine. `c` is the coroutine instance that
  ## will be returned by the `resume <#resume,T>`_ that resumed execution
  ## of the instance.

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
  elif c.running:
    csRunning
  else:
    csPending

{.push checks: off.}

proc resume*[T: Coroutine](c: T): T {.discardable.} =
  ## Yields control to the given suspendend coroutine instance `c`, which is
  ## then executed until the first suspension point is reached. Returns the
  ## coroutine instance that the coroutine yielded control to (may be nil).
  # TODO: prevent resuming running coroutine instance
  c.running = true
  result = T(c.fn(c))
  c.running = false

{.pop.}

proc finish*[T](c: sink Coroutine[T]): T =
  case c.status
  of csPending:
    when T isnot void:
      result = move c.result
    c.state = -3 # done
  else:
    raise CoroutineError.newException("not pending")

proc unwrap*(c: sink CoroutineBase): Exception =
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
  var c = c
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
      finish(c)
    else:
      result = finish(c)
  of csRunning:
    # the coroutine must have yielded to a another, running coroutine
    # TODO: report an error
    discard
  of csFinished:
    # the coroutine must have yielded to a finished coroutine
    # TODO: report an error
    discard
