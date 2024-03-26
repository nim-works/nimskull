discard """
  description: '''
    Ensure that inlining an inline iterator into a coroutine works. Suspending
    within the for-loop must work
  '''
  output: "1"
"""

# echo is used to prevent doAssert from interfering with the
# transformation

iterator iter(): int {.inline.} =
  var x = 1
  yield x
  echo x

proc test() {.coroutine.} =
  for it in iter():
    suspend(self)

trampoline test()
