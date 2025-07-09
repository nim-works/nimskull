discard """
  description: '''
    An inner procedure using the .closure calling convention may be used in
    a separate, non-.closure inner routine.
  '''
  knownIssue: '''
    Lambda-lifting assumes .closure means "closes over something", requiring
    sempass2 to enforce the .closure calling covention on the callee/using
    routine
  '''
"""

proc test() =
  proc inner() {.closure.} =
    # uses closure calling convention, but doesn't close over anything
    discard

  proc inner2() {.nimcall.} =
    inner()

  inner2()

test()
