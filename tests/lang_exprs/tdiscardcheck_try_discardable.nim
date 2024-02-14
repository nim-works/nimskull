discard """
  description: "Test to ensure discardable items are _not_ checked"
"""

block try_should_work_fine:
  proc foo(): int {.discardable.} =
    42

  try:
    foo()
  finally:
    discard

block except_should_also_work:
  proc foo(): int {.discardable.} =
    42

  try:
    discard
  except:
    foo()
  finally:
    discard

block finally_should_also_work:
  proc foo(): int {.discardable.} =
    42

  try:
    discard
  except:
    discard
  finally:
    foo()