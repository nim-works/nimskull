discard """
  targets: c
  description: '''
    Regression tests related for C code-generator issues related to
    goto-exceptions
  '''
"""

block missing_exception_control_flow:
  # no if-error-goto-exit was injected after the call to a closure procedure
  # using RVO. At the time of writing, arrays always use RVO when using the C
  # target.
  proc outer(p: ptr int): array[4, int] =
    proc inner(): array[4, int] =
      discard p # capture something
      raise CatchableError.newException("")

    result = inner()
    p[] = 1 # `p` is nil; the assignment must never be reached

  var caught = false
  try:
    discard outer(nil)
  except CatchableError:
    caught = true

  doAssert caught