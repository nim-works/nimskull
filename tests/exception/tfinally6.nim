discard """
  description: '''
    Multiple tests regarding ``finally`` interaction with exception handlers
    and raised exceptions.
  '''
  knownIssue.c js: "The current exception is not properly cleared"
"""

var steps: seq[int]

template test(call: untyped, expect: untyped) =
  steps = @[] # reset the step list
  {.line.}:
    call
    doAssert steps == expect, $steps
    doAssert getCurrentException().isNil, "current exception wasn't cleared"

# ------ the tests follow ------

proc simpleFinally() =
  try:
    try:
      raise ValueError.newException("a")
    finally:
      steps.add 1
    steps.add 2
  except ValueError as e:
    steps.add 3
    doAssert e.msg == "a"

test simpleFinally(), [1, 3]

proc raiseFromFinally() =
  try:
    try:
      raise ValueError.newException("a")
    finally:
      steps.add 1
      raise ValueError.newException("b")
    doAssert false, "unreachable"
  except ValueError as e:
    # the exception raised in the finally clause overrides the one raised
    # earlier
    steps.add 2
    doAssert e.msg == "b"
    doAssert getCurrentException() == e

  steps.add 3

test raiseFromFinally(), [1, 2, 3]

proc reraiseFromFinally() =
  try:
    try:
      raise ValueError.newException("a")
    finally:
      steps.add 1
      # abort the exception but immediately re-raise it
      raise
    doAssert false, "unreachable"
  except ValueError as e:
    steps.add 2
    doAssert e.msg == "a"
    doAssert getCurrentException() == e

  steps.add 3

test reraiseFromFinally(), [1, 2, 3]

proc exceptionInFinally() =
  ## Raise, and fully handle, an exception within a finally clause that was
  ## entered through exceptional control-flow.
  try:
    try:
      raise ValueError.newException("a")
    finally:
      steps.add 1
      try:
        raise ValueError.newException("b")
      except ValueError as e:
        steps.add 2
        doAssert e.msg == "b"
        doAssert getCurrentException() == e

      steps.add 3
      # the current exception must be the one with which the finally section
      # was entered
      doAssert getCurrentException().msg == "a"

    doAssert false, "unreachable"
  except ValueError as e:
    steps.add 4
    doAssert e.msg == "a"

  steps.add 5

test exceptionInFinally(), [1, 2, 3, 4, 5]

proc leaveFinally1() =
  ## Ensure that exiting a finally clause entered through exceptional control-
  ## flow via unstructured control-flow (break) works and properly clears the
  ## current exception.
  block exit:
    try:
      raise ValueError.newException("a")
    finally:
      steps.add 1
      doAssert getCurrentException().msg == "a"
      break exit
    doAssert false, "unreachable"

  steps.add 2

test leaveFinally1(), [1, 2]

proc leaveFinally2() =
  ## Ensure that aborting an exception raised within a finally clause entered
  ## through exceptional control-flow doesn't interfere with the original
  ## exception.
  try:
    try:
      raise ValueError.newException("a")
    finally:
      block exit:
        steps.add 1
        try:
          raise ValueError.newException("b")
        finally:
          steps.add 2
          # discards the in-flight exception 'b'
          break exit
        doAssert false, "unreachable"

      steps.add 3
      # the current exception must be the one the finally was entered with:
      doAssert getCurrentException().msg == "a"
      # unwinding continues as usual
    doAssert false, "unreachable"
  except ValueError as e:
    steps.add 4
    doAssert e.msg == "a"
    doAssert getCurrentException() == e

test leaveFinally2(), [1, 2, 3, 4]

proc leaveFinally3(doExit: bool) =
  ## Ensure that aborting an exception in a finally clause still visits all
  ## enclosing finally clauses, and that the finally clauses observe the
  ## correct current exception.
  block exit:
    try:
      try:
        raise ValueError.newException("a")
      finally:
        steps.add 1
        try:
          if doExit: # obfuscate the break
            break exit
        finally:
          steps.add 2
          # the current exception must still be set
          doAssert getCurrentException().msg == "a"
        doAssert false, "unreachable"
      doAssert false, "unreachable"
    finally:
      steps.add 5
      # the finally section is not part of the inner try statement, so the
      # current exception is nil
      doAssert getCurrentException() == nil
    doAssert false, "unreachable"

test leaveFinally3(true), [1, 2, 5]
