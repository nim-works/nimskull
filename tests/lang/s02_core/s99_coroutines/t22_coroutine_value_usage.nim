discard """
  output: "here\n1\n2"
"""

## A coroutine value can be used the same way as the immediate symbol of a
## coroutine.

proc coro() {.coroutine.} =
  echo "here"

let c = coro
resume(c())

## The same is true for closure coroutine values.

proc make(): (proc() {.coroutine.}) =
  var x = 1
  proc coro() {.coroutine.} =
    echo x
    inc x

  result = coro

let cc = make()
resume(cc())
resume(cc())
