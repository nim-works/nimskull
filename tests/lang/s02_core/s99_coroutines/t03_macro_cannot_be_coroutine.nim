discard """
  description: "Macros cannot be coroutines"
  action: reject
"""

# XXX: perhaps too restrictive; it's not impossible to implement

macro m() {.coroutine.} =
  discard
