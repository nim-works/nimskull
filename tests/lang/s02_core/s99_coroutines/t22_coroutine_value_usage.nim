discard """
  output: "here\ntrampoline\nhere\n1\ntrampoline\n2"
"""

## A coroutine value can be used the same way as the immediate symbol of a
## coroutine.

proc coro() {.coroutine.} =
  echo "here"

let c = coro

# with launch:
resume(launch c())

# with trampoline:
proc trampoline(x: sink Coroutine) =
  echo "trampoline"
  resume(x)

c() # invokes trampoline

## The same is true for closure coroutine values.

proc make(): (proc() {.coroutine.}) =
  var x = 1
  proc coro() {.coroutine.} =
    echo x
    inc x

  result = coro

let cc = make()
# with launch:
resume(launch cc())

# with trampoline:
cc()
