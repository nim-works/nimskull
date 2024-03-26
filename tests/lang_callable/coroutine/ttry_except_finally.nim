discard """
  description: '''
    Ensure that a basic try/except/finally within a coroutine works, with each
    clause supporting being suspended from
  '''
  output: "1\n2\n3"
"""

import std/vmutils

static:
  vmTrace(true)
  proc test() {.coroutine.} =
    try:
      echo "1"
      suspend(self)
      raise CatchableError.newException("a")
    except CatchableError as e:
      echo "2"
      suspend(self)
    finally:
      echo "3"
      suspend(self)

  trampoline test()
