discard """
  output: "Coroutine[void]\ncsRunning"
"""

## To access the active *instance* within the body of a coroutine, the hidden
## ``self`` parameter is made available.

# XXX: not a good solution, either the ``self`` parameter should be explicit
#      (somehow), or there should be a magic procedure

proc coro() {.coroutine.} =
  # the self parameter is of type ``Coroutine[void]``
  echo typeof(self)
  echo self.status

var instance = launch coro()
resume(instance)
