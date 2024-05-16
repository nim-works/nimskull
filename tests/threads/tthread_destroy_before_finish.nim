discard """
  description: '''
    Ensure that detaching a thread (destroying the handle before the spawned
    thread finishes) works
  '''
  output: ""
  joinable: false
"""

var step = 0
proc whatever() =
  # busy-loop until the thread is detached:
  while atomicLoadN(addr step, ATOMIC_SEQ_CST) < 1:
    discard

  # thread is detached, increment the counter
  discard atomicInc(step, 1, ATOMIC_SEQ_CST)

var thread: Thread[void]
createThread(thread, whatever)

# Simulated: variable going out of scope
thread.reset()

# signal that the thread is detached now:
discard atomicInc(step, 1, ATOMIC_SEQ_CST)

# wait until the thread is done:
while atomicLoadN(addr step, ATOMIC_SEQ_CST) < 2:
  discard

