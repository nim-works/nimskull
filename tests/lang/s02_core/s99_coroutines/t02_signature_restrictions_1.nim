discard """
"""

## Except for `openArray` and `var` types, there are no restrictions on the
## number or type of the parameters, compared to non-coroutine procedures.

proc a(x: int, y: seq[float], z: string) {.coroutine.} =
  discard

## Coroutines cannot return views, but there are no other restrictions.

proc b(): float {.coroutine.} =
  discard